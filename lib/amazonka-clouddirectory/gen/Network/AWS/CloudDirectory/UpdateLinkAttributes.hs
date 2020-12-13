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
    ulaDirectoryARN,
    ulaAttributeUpdates,
    ulaTypedLinkSpecifier,

    -- * Destructuring the response
    UpdateLinkAttributesResponse (..),
    mkUpdateLinkAttributesResponse,

    -- ** Response lenses
    ularsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateLinkAttributes' smart constructor.
data UpdateLinkAttributes = UpdateLinkAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    directoryARN :: Lude.Text,
    -- | The attributes update structure.
    attributeUpdates :: [LinkAttributeUpdate],
    -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLinkAttributes' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'attributeUpdates' - The attributes update structure.
-- * 'typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
mkUpdateLinkAttributes ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  UpdateLinkAttributes
mkUpdateLinkAttributes pDirectoryARN_ pTypedLinkSpecifier_ =
  UpdateLinkAttributes'
    { directoryARN = pDirectoryARN_,
      attributeUpdates = Lude.mempty,
      typedLinkSpecifier = pTypedLinkSpecifier_
    }

-- | The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulaDirectoryARN :: Lens.Lens' UpdateLinkAttributes Lude.Text
ulaDirectoryARN = Lens.lens (directoryARN :: UpdateLinkAttributes -> Lude.Text) (\s a -> s {directoryARN = a} :: UpdateLinkAttributes)
{-# DEPRECATED ulaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulaAttributeUpdates :: Lens.Lens' UpdateLinkAttributes [LinkAttributeUpdate]
ulaAttributeUpdates = Lens.lens (attributeUpdates :: UpdateLinkAttributes -> [LinkAttributeUpdate]) (\s a -> s {attributeUpdates = a} :: UpdateLinkAttributes)
{-# DEPRECATED ulaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulaTypedLinkSpecifier :: Lens.Lens' UpdateLinkAttributes TypedLinkSpecifier
ulaTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: UpdateLinkAttributes -> TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: UpdateLinkAttributes)
{-# DEPRECATED ulaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Lude.AWSRequest UpdateLinkAttributes where
  type Rs UpdateLinkAttributes = UpdateLinkAttributesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateLinkAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateLinkAttributes where
  toHeaders UpdateLinkAttributes' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON UpdateLinkAttributes where
  toJSON UpdateLinkAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeUpdates" Lude..= attributeUpdates),
            Lude.Just ("TypedLinkSpecifier" Lude..= typedLinkSpecifier)
          ]
      )

instance Lude.ToPath UpdateLinkAttributes where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/update"

instance Lude.ToQuery UpdateLinkAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateLinkAttributesResponse' smart constructor.
newtype UpdateLinkAttributesResponse = UpdateLinkAttributesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLinkAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateLinkAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateLinkAttributesResponse
mkUpdateLinkAttributesResponse pResponseStatus_ =
  UpdateLinkAttributesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ularsResponseStatus :: Lens.Lens' UpdateLinkAttributesResponse Lude.Int
ularsResponseStatus = Lens.lens (responseStatus :: UpdateLinkAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateLinkAttributesResponse)
{-# DEPRECATED ularsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
