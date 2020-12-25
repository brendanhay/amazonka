{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given typed link’s attributes. Attributes to be updated must not contribute to the typed link’s identity, as defined by its @IdentityAttributeOrder@ .
module Network.AWS.CloudDirectory.UpdateLinkAttributes
  ( -- * Creating a request
    UpdateLinkAttributes (..),
    mkUpdateLinkAttributes,

    -- ** Request lenses
    ulaDirectoryArn,
    ulaTypedLinkSpecifier,
    ulaAttributeUpdates,

    -- * Destructuring the response
    UpdateLinkAttributesResponse (..),
    mkUpdateLinkAttributesResponse,

    -- ** Response lenses
    ularrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateLinkAttributes' smart constructor.
data UpdateLinkAttributes = UpdateLinkAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    directoryArn :: Types.Arn,
    -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: Types.TypedLinkSpecifier,
    -- | The attributes update structure.
    attributeUpdates :: [Types.LinkAttributeUpdate]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateLinkAttributes' value with any optional fields omitted.
mkUpdateLinkAttributes ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'typedLinkSpecifier'
  Types.TypedLinkSpecifier ->
  UpdateLinkAttributes
mkUpdateLinkAttributes directoryArn typedLinkSpecifier =
  UpdateLinkAttributes'
    { directoryArn,
      typedLinkSpecifier,
      attributeUpdates = Core.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulaDirectoryArn :: Lens.Lens' UpdateLinkAttributes Types.Arn
ulaDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED ulaDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulaTypedLinkSpecifier :: Lens.Lens' UpdateLinkAttributes Types.TypedLinkSpecifier
ulaTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# DEPRECATED ulaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | The attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulaAttributeUpdates :: Lens.Lens' UpdateLinkAttributes [Types.LinkAttributeUpdate]
ulaAttributeUpdates = Lens.field @"attributeUpdates"
{-# DEPRECATED ulaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

instance Core.FromJSON UpdateLinkAttributes where
  toJSON UpdateLinkAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TypedLinkSpecifier" Core..= typedLinkSpecifier),
            Core.Just ("AttributeUpdates" Core..= attributeUpdates)
          ]
      )

instance Core.AWSRequest UpdateLinkAttributes where
  type Rs UpdateLinkAttributes = UpdateLinkAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            "/amazonclouddirectory/2017-01-11/typedlink/attributes/update",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLinkAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateLinkAttributesResponse' smart constructor.
newtype UpdateLinkAttributesResponse = UpdateLinkAttributesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLinkAttributesResponse' value with any optional fields omitted.
mkUpdateLinkAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateLinkAttributesResponse
mkUpdateLinkAttributesResponse responseStatus =
  UpdateLinkAttributesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ularrsResponseStatus :: Lens.Lens' UpdateLinkAttributesResponse Core.Int
ularrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ularrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
