{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.RevokeGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the specified grant for the specified customer master key (CMK). You can revoke a grant to actively deny operations that depend on it.
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter.
module Network.AWS.KMS.RevokeGrant
  ( -- * Creating a request
    RevokeGrant (..),
    mkRevokeGrant,

    -- ** Request lenses
    rKeyId,
    rGrantId,

    -- * Destructuring the response
    RevokeGrantResponse (..),
    mkRevokeGrantResponse,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeGrant' smart constructor.
data RevokeGrant = RevokeGrant'
  { -- | A unique identifier for the customer master key associated with the grant.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    keyId :: Types.KeyId,
    -- | Identifier of the grant to be revoked.
    grantId :: Types.GrantId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeGrant' value with any optional fields omitted.
mkRevokeGrant ::
  -- | 'keyId'
  Types.KeyId ->
  -- | 'grantId'
  Types.GrantId ->
  RevokeGrant
mkRevokeGrant keyId grantId = RevokeGrant' {keyId, grantId}

-- | A unique identifier for the customer master key associated with the grant.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKeyId :: Lens.Lens' RevokeGrant Types.KeyId
rKeyId = Lens.field @"keyId"
{-# DEPRECATED rKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Identifier of the grant to be revoked.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGrantId :: Lens.Lens' RevokeGrant Types.GrantId
rGrantId = Lens.field @"grantId"
{-# DEPRECATED rGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

instance Core.FromJSON RevokeGrant where
  toJSON RevokeGrant {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("GrantId" Core..= grantId)
          ]
      )

instance Core.AWSRequest RevokeGrant where
  type Rs RevokeGrant = RevokeGrantResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.RevokeGrant")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RevokeGrantResponse'

-- | /See:/ 'mkRevokeGrantResponse' smart constructor.
data RevokeGrantResponse = RevokeGrantResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeGrantResponse' value with any optional fields omitted.
mkRevokeGrantResponse ::
  RevokeGrantResponse
mkRevokeGrantResponse = RevokeGrantResponse'
