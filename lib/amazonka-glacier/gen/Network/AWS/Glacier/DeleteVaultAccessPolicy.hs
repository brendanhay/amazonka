{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteVaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the access policy associated with the specified vault. The operation is eventually consistent; that is, it might take some time for Amazon S3 Glacier to completely remove the access policy, and you might still see the effect of the policy for a short time after you send the delete request.
--
-- This operation is idempotent. You can invoke delete multiple times, even if there is no policy associated with the vault. For more information about vault access policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> . 
module Network.AWS.Glacier.DeleteVaultAccessPolicy
    (
    -- * Creating a request
      DeleteVaultAccessPolicy (..)
    , mkDeleteVaultAccessPolicy
    -- ** Request lenses
    , dvapAccountId
    , dvapVaultName

    -- * Destructuring the response
    , DeleteVaultAccessPolicyResponse (..)
    , mkDeleteVaultAccessPolicyResponse
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DeleteVaultAccessPolicy input.
--
-- /See:/ 'mkDeleteVaultAccessPolicy' smart constructor.
data DeleteVaultAccessPolicy = DeleteVaultAccessPolicy'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVaultAccessPolicy' value with any optional fields omitted.
mkDeleteVaultAccessPolicy
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> DeleteVaultAccessPolicy
mkDeleteVaultAccessPolicy accountId vaultName
  = DeleteVaultAccessPolicy'{accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvapAccountId :: Lens.Lens' DeleteVaultAccessPolicy Core.Text
dvapAccountId = Lens.field @"accountId"
{-# INLINEABLE dvapAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvapVaultName :: Lens.Lens' DeleteVaultAccessPolicy Core.Text
dvapVaultName = Lens.field @"vaultName"
{-# INLINEABLE dvapVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

instance Core.ToQuery DeleteVaultAccessPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVaultAccessPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVaultAccessPolicy where
        type Rs DeleteVaultAccessPolicy = DeleteVaultAccessPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/access-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteVaultAccessPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVaultAccessPolicyResponse' smart constructor.
data DeleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVaultAccessPolicyResponse' value with any optional fields omitted.
mkDeleteVaultAccessPolicyResponse
    :: DeleteVaultAccessPolicyResponse
mkDeleteVaultAccessPolicyResponse
  = DeleteVaultAccessPolicyResponse'
