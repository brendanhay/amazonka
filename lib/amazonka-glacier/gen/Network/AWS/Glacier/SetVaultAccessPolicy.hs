{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.SetVaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures an access policy for a vault and will overwrite an existing policy. To configure a vault access policy, send a PUT request to the @access-policy@ subresource of the vault. An access policy is specific to a vault and is also called a vault subresource. You can set one access policy per vault and the policy can be up to 20 KB in size. For more information about vault access policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> . 
module Network.AWS.Glacier.SetVaultAccessPolicy
    (
    -- * Creating a request
      SetVaultAccessPolicy (..)
    , mkSetVaultAccessPolicy
    -- ** Request lenses
    , svapAccountId
    , svapVaultName
    , svapPolicy

    -- * Destructuring the response
    , SetVaultAccessPolicyResponse (..)
    , mkSetVaultAccessPolicyResponse
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | SetVaultAccessPolicy input.
--
-- /See:/ 'mkSetVaultAccessPolicy' smart constructor.
data SetVaultAccessPolicy = SetVaultAccessPolicy'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , policy :: Core.Maybe Types.VaultAccessPolicy
    -- ^ The vault access policy as a JSON string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetVaultAccessPolicy' value with any optional fields omitted.
mkSetVaultAccessPolicy
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> SetVaultAccessPolicy
mkSetVaultAccessPolicy accountId vaultName
  = SetVaultAccessPolicy'{accountId, vaultName,
                          policy = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svapAccountId :: Lens.Lens' SetVaultAccessPolicy Core.Text
svapAccountId = Lens.field @"accountId"
{-# INLINEABLE svapAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svapVaultName :: Lens.Lens' SetVaultAccessPolicy Core.Text
svapVaultName = Lens.field @"vaultName"
{-# INLINEABLE svapVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | The vault access policy as a JSON string.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svapPolicy :: Lens.Lens' SetVaultAccessPolicy (Core.Maybe Types.VaultAccessPolicy)
svapPolicy = Lens.field @"policy"
{-# INLINEABLE svapPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

instance Core.ToQuery SetVaultAccessPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetVaultAccessPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetVaultAccessPolicy where
        toJSON SetVaultAccessPolicy{..}
          = Core.object (Core.catMaybes [("policy" Core..=) Core.<$> policy])

instance Core.AWSRequest SetVaultAccessPolicy where
        type Rs SetVaultAccessPolicy = SetVaultAccessPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/access-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetVaultAccessPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetVaultAccessPolicyResponse' smart constructor.
data SetVaultAccessPolicyResponse = SetVaultAccessPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetVaultAccessPolicyResponse' value with any optional fields omitted.
mkSetVaultAccessPolicyResponse
    :: SetVaultAccessPolicyResponse
mkSetVaultAccessPolicyResponse = SetVaultAccessPolicyResponse'
