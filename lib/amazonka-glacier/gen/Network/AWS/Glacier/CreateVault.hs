{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CreateVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a new vault with the specified name. The name of the vault must be unique within a region for an AWS account. You can create up to 1,000 vaults per account. If you need to create more vaults, contact Amazon S3 Glacier.
--
-- You must use the following guidelines when naming a vault.
--
--     * Names can be between 1 and 255 characters long.
--
--
--     * Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), and '.' (period).
--
--
-- This operation is idempotent.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html Creating a Vault in Amazon Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html Create Vault > in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.CreateVault
  ( -- * Creating a request
    CreateVault (..),
    mkCreateVault,

    -- ** Request lenses
    cvAccountId,
    cvVaultName,

    -- * Destructuring the response
    CreateVaultResponse (..),
    mkCreateVaultResponse,

    -- ** Response lenses
    cvrrsLocation,
    cvrrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to create a vault.
--
-- /See:/ 'mkCreateVault' smart constructor.
data CreateVault = CreateVault'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVault' value with any optional fields omitted.
mkCreateVault ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  CreateVault
mkCreateVault accountId vaultName =
  CreateVault' {accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvAccountId :: Lens.Lens' CreateVault Types.String
cvAccountId = Lens.field @"accountId"
{-# DEPRECATED cvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVaultName :: Lens.Lens' CreateVault Types.String
cvVaultName = Lens.field @"vaultName"
{-# DEPRECATED cvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.FromJSON CreateVault where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateVault where
  type Rs CreateVault = CreateVaultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateVaultResponse'
            Core.<$> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkCreateVaultResponse' smart constructor.
data CreateVaultResponse = CreateVaultResponse'
  { -- | The URI of the vault that was created.
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVaultResponse' value with any optional fields omitted.
mkCreateVaultResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateVaultResponse
mkCreateVaultResponse responseStatus =
  CreateVaultResponse' {location = Core.Nothing, responseStatus}

-- | The URI of the vault that was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsLocation :: Lens.Lens' CreateVaultResponse (Core.Maybe Types.String)
cvrrsLocation = Lens.field @"location"
{-# DEPRECATED cvrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsResponseStatus :: Lens.Lens' CreateVaultResponse Core.Int
cvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
