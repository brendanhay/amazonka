{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.RetireGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retires a grant. To clean up, you can retire a grant when you're done using it. You should revoke a grant when you intend to actively deny operations that depend on it. The following are permitted to call this API:
--
--
--     * The AWS account (root user) under which the grant was created
--
--
--     * The @RetiringPrincipal@ , if present in the grant
--
--
--     * The @GranteePrincipal@ , if @RetireGrant@ is an operation specified in the grant
--
--
-- You must identify the grant to retire by its grant token or by a combination of the grant ID and the Amazon Resource Name (ARN) of the customer master key (CMK). A grant token is a unique variable-length base64-encoded string. A grant ID is a 64 character unique identifier of a grant. The 'CreateGrant' operation returns both.
module Network.AWS.KMS.RetireGrant
  ( -- * Creating a request
    RetireGrant (..),
    mkRetireGrant,

    -- ** Request lenses
    rgGrantId,
    rgGrantToken,
    rgKeyId,

    -- * Destructuring the response
    RetireGrantResponse (..),
    mkRetireGrantResponse,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRetireGrant' smart constructor.
data RetireGrant = RetireGrant'
  { -- | Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.
    --
    --
    --     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
    grantId :: Core.Maybe Types.GrantId,
    -- | Token that identifies the grant to be retired.
    grantToken :: Core.Maybe Types.GrantToken,
    -- | The Amazon Resource Name (ARN) of the CMK associated with the grant.
    --
    -- For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    keyId :: Core.Maybe Types.KeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetireGrant' value with any optional fields omitted.
mkRetireGrant ::
  RetireGrant
mkRetireGrant =
  RetireGrant'
    { grantId = Core.Nothing,
      grantToken = Core.Nothing,
      keyId = Core.Nothing
    }

-- | Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.
--
--
--     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
--
--
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGrantId :: Lens.Lens' RetireGrant (Core.Maybe Types.GrantId)
rgGrantId = Lens.field @"grantId"
{-# DEPRECATED rgGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

-- | Token that identifies the grant to be retired.
--
-- /Note:/ Consider using 'grantToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGrantToken :: Lens.Lens' RetireGrant (Core.Maybe Types.GrantToken)
rgGrantToken = Lens.field @"grantToken"
{-# DEPRECATED rgGrantToken "Use generic-lens or generic-optics with 'grantToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK associated with the grant.
--
-- For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgKeyId :: Lens.Lens' RetireGrant (Core.Maybe Types.KeyId)
rgKeyId = Lens.field @"keyId"
{-# DEPRECATED rgKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Core.FromJSON RetireGrant where
  toJSON RetireGrant {..} =
    Core.object
      ( Core.catMaybes
          [ ("GrantId" Core..=) Core.<$> grantId,
            ("GrantToken" Core..=) Core.<$> grantToken,
            ("KeyId" Core..=) Core.<$> keyId
          ]
      )

instance Core.AWSRequest RetireGrant where
  type Rs RetireGrant = RetireGrantResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.RetireGrant")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RetireGrantResponse'

-- | /See:/ 'mkRetireGrantResponse' smart constructor.
data RetireGrantResponse = RetireGrantResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetireGrantResponse' value with any optional fields omitted.
mkRetireGrantResponse ::
  RetireGrantResponse
mkRetireGrantResponse = RetireGrantResponse'
