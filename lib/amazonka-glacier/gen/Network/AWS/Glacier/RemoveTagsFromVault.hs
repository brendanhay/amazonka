{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RemoveTagsFromVault (..)
    , mkRemoveTagsFromVault
    -- ** Request lenses
    , rtfvAccountId
    , rtfvVaultName
    , rtfvTagKeys

    -- * Destructuring the response
    , RemoveTagsFromVaultResponse (..)
    , mkRemoveTagsFromVaultResponse
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input value for @RemoveTagsFromVaultInput@ .
--
-- /See:/ 'mkRemoveTagsFromVault' smart constructor.
data RemoveTagsFromVault = RemoveTagsFromVault'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , tagKeys :: Core.Maybe [Core.Text]
    -- ^ A list of tag keys. Each corresponding tag is removed from the vault.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromVault' value with any optional fields omitted.
mkRemoveTagsFromVault
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> RemoveTagsFromVault
mkRemoveTagsFromVault accountId vaultName
  = RemoveTagsFromVault'{accountId, vaultName,
                         tagKeys = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvAccountId :: Lens.Lens' RemoveTagsFromVault Core.Text
rtfvAccountId = Lens.field @"accountId"
{-# INLINEABLE rtfvAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvVaultName :: Lens.Lens' RemoveTagsFromVault Core.Text
rtfvVaultName = Lens.field @"vaultName"
{-# INLINEABLE rtfvVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | A list of tag keys. Each corresponding tag is removed from the vault.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvTagKeys :: Lens.Lens' RemoveTagsFromVault (Core.Maybe [Core.Text])
rtfvTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtfvTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTagsFromVault where
        toQuery RemoveTagsFromVault{..}
          = Core.toQueryPair "operation=remove" ("" :: Core.Text)

instance Core.ToHeaders RemoveTagsFromVault where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RemoveTagsFromVault where
        toJSON RemoveTagsFromVault{..}
          = Core.object
              (Core.catMaybes [("TagKeys" Core..=) Core.<$> tagKeys])

instance Core.AWSRequest RemoveTagsFromVault where
        type Rs RemoveTagsFromVault = RemoveTagsFromVaultResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/tags",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RemoveTagsFromVaultResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveTagsFromVaultResponse' smart constructor.
data RemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromVaultResponse' value with any optional fields omitted.
mkRemoveTagsFromVaultResponse
    :: RemoveTagsFromVaultResponse
mkRemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
