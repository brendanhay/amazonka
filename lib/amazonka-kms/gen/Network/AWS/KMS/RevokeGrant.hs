{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RevokeGrant (..)
    , mkRevokeGrant
    -- ** Request lenses
    , rKeyId
    , rGrantId

    -- * Destructuring the response
    , RevokeGrantResponse (..)
    , mkRevokeGrantResponse
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeGrant' smart constructor.
data RevokeGrant = RevokeGrant'
  { keyId :: Types.KeyId
    -- ^ A unique identifier for the customer master key associated with the grant.
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
  , grantId :: Types.GrantId
    -- ^ Identifier of the grant to be revoked.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeGrant' value with any optional fields omitted.
mkRevokeGrant
    :: Types.KeyId -- ^ 'keyId'
    -> Types.GrantId -- ^ 'grantId'
    -> RevokeGrant
mkRevokeGrant keyId grantId = RevokeGrant'{keyId, grantId}

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
{-# INLINEABLE rKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Identifier of the grant to be revoked.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGrantId :: Lens.Lens' RevokeGrant Types.GrantId
rGrantId = Lens.field @"grantId"
{-# INLINEABLE rGrantId #-}
{-# DEPRECATED grantId "Use generic-lens or generic-optics with 'grantId' instead"  #-}

instance Core.ToQuery RevokeGrant where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RevokeGrant where
        toHeaders RevokeGrant{..}
          = Core.pure ("X-Amz-Target", "TrentService.RevokeGrant") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RevokeGrant where
        toJSON RevokeGrant{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("GrantId" Core..= grantId)])

instance Core.AWSRequest RevokeGrant where
        type Rs RevokeGrant = RevokeGrantResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RevokeGrantResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeGrantResponse' smart constructor.
data RevokeGrantResponse = RevokeGrantResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeGrantResponse' value with any optional fields omitted.
mkRevokeGrantResponse
    :: RevokeGrantResponse
mkRevokeGrantResponse = RevokeGrantResponse'
