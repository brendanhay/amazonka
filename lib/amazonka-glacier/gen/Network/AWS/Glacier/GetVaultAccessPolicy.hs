{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @access-policy@ subresource set on the vault; for more information on setting this subresource, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html Set Vault Access Policy (PUT access-policy)> . If there is no access policy set on the vault, the operation returns a @404 Not found@ error. For more information about vault access policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> .
module Network.AWS.Glacier.GetVaultAccessPolicy
  ( -- * Creating a request
    GetVaultAccessPolicy (..),
    mkGetVaultAccessPolicy,

    -- ** Request lenses
    gvapAccountId,
    gvapVaultName,

    -- * Destructuring the response
    GetVaultAccessPolicyResponse (..),
    mkGetVaultAccessPolicyResponse,

    -- ** Response lenses
    gvaprrsPolicy,
    gvaprrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for GetVaultAccessPolicy.
--
-- /See:/ 'mkGetVaultAccessPolicy' smart constructor.
data GetVaultAccessPolicy = GetVaultAccessPolicy'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVaultAccessPolicy' value with any optional fields omitted.
mkGetVaultAccessPolicy ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  GetVaultAccessPolicy
mkGetVaultAccessPolicy accountId vaultName =
  GetVaultAccessPolicy' {accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvapAccountId :: Lens.Lens' GetVaultAccessPolicy Types.String
gvapAccountId = Lens.field @"accountId"
{-# DEPRECATED gvapAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvapVaultName :: Lens.Lens' GetVaultAccessPolicy Types.String
gvapVaultName = Lens.field @"vaultName"
{-# DEPRECATED gvapVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.AWSRequest GetVaultAccessPolicy where
  type Rs GetVaultAccessPolicy = GetVaultAccessPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/access-policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVaultAccessPolicyResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Output for GetVaultAccessPolicy.
--
-- /See:/ 'mkGetVaultAccessPolicyResponse' smart constructor.
data GetVaultAccessPolicyResponse = GetVaultAccessPolicyResponse'
  { -- | Contains the returned vault access policy as a JSON string.
    policy :: Core.Maybe Types.VaultAccessPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVaultAccessPolicyResponse' value with any optional fields omitted.
mkGetVaultAccessPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetVaultAccessPolicyResponse
mkGetVaultAccessPolicyResponse responseStatus =
  GetVaultAccessPolicyResponse'
    { policy = Core.Nothing,
      responseStatus
    }

-- | Contains the returned vault access policy as a JSON string.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvaprrsPolicy :: Lens.Lens' GetVaultAccessPolicyResponse (Core.Maybe Types.VaultAccessPolicy)
gvaprrsPolicy = Lens.field @"policy"
{-# DEPRECATED gvaprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvaprrsResponseStatus :: Lens.Lens' GetVaultAccessPolicyResponse Core.Int
gvaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gvaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
