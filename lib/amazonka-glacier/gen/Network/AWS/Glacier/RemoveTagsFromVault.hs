{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.RemoveTagsFromVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes one or more tags from the set of tags attached to a vault. For more information about tags, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon S3 Glacier Resources> . This operation is idempotent. The operation will be successful, even if there are no tags attached to the vault.
module Network.AWS.Glacier.RemoveTagsFromVault
  ( -- * Creating a request
    RemoveTagsFromVault (..),
    mkRemoveTagsFromVault,

    -- ** Request lenses
    rtfvAccountId,
    rtfvVaultName,
    rtfvTagKeys,

    -- * Destructuring the response
    RemoveTagsFromVaultResponse (..),
    mkRemoveTagsFromVaultResponse,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input value for @RemoveTagsFromVaultInput@ .
--
-- /See:/ 'mkRemoveTagsFromVault' smart constructor.
data RemoveTagsFromVault = RemoveTagsFromVault'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String,
    -- | A list of tag keys. Each corresponding tag is removed from the vault.
    tagKeys :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromVault' value with any optional fields omitted.
mkRemoveTagsFromVault ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  RemoveTagsFromVault
mkRemoveTagsFromVault accountId vaultName =
  RemoveTagsFromVault'
    { accountId,
      vaultName,
      tagKeys = Core.Nothing
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvAccountId :: Lens.Lens' RemoveTagsFromVault Types.String
rtfvAccountId = Lens.field @"accountId"
{-# DEPRECATED rtfvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvVaultName :: Lens.Lens' RemoveTagsFromVault Types.String
rtfvVaultName = Lens.field @"vaultName"
{-# DEPRECATED rtfvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | A list of tag keys. Each corresponding tag is removed from the vault.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvTagKeys :: Lens.Lens' RemoveTagsFromVault (Core.Maybe [Types.String])
rtfvTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED rtfvTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON RemoveTagsFromVault where
  toJSON RemoveTagsFromVault {..} =
    Core.object
      (Core.catMaybes [("TagKeys" Core..=) Core.<$> tagKeys])

instance Core.AWSRequest RemoveTagsFromVault where
  type Rs RemoveTagsFromVault = RemoveTagsFromVaultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/tags")
            ),
        Core._rqQuery = Core.pure ("operation=remove", ""),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RemoveTagsFromVaultResponse'

-- | /See:/ 'mkRemoveTagsFromVaultResponse' smart constructor.
data RemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromVaultResponse' value with any optional fields omitted.
mkRemoveTagsFromVaultResponse ::
  RemoveTagsFromVaultResponse
mkRemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
