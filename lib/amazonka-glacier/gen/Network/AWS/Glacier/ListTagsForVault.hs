{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListTagsForVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all the tags attached to a vault. The operation returns an empty map if there are no tags. For more information about tags, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon S3 Glacier Resources> .
module Network.AWS.Glacier.ListTagsForVault
  ( -- * Creating a request
    ListTagsForVault (..),
    mkListTagsForVault,

    -- ** Request lenses
    ltfvAccountId,
    ltfvVaultName,

    -- * Destructuring the response
    ListTagsForVaultResponse (..),
    mkListTagsForVaultResponse,

    -- ** Response lenses
    ltfvrrsTags,
    ltfvrrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input value for @ListTagsForVaultInput@ .
--
-- /See:/ 'mkListTagsForVault' smart constructor.
data ListTagsForVault = ListTagsForVault'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForVault' value with any optional fields omitted.
mkListTagsForVault ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  ListTagsForVault
mkListTagsForVault accountId vaultName =
  ListTagsForVault' {accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvAccountId :: Lens.Lens' ListTagsForVault Types.String
ltfvAccountId = Lens.field @"accountId"
{-# DEPRECATED ltfvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvVaultName :: Lens.Lens' ListTagsForVault Types.String
ltfvVaultName = Lens.field @"vaultName"
{-# DEPRECATED ltfvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.AWSRequest ListTagsForVault where
  type Rs ListTagsForVault = ListTagsForVaultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/tags")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForVaultResponse'
            Core.<$> (x Core..:? "Tags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListTagsForVaultResponse' smart constructor.
data ListTagsForVaultResponse = ListTagsForVaultResponse'
  { -- | The tags attached to the vault. Each tag is composed of a key and a value.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForVaultResponse' value with any optional fields omitted.
mkListTagsForVaultResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTagsForVaultResponse
mkListTagsForVaultResponse responseStatus =
  ListTagsForVaultResponse' {tags = Core.Nothing, responseStatus}

-- | The tags attached to the vault. Each tag is composed of a key and a value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvrrsTags :: Lens.Lens' ListTagsForVaultResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltfvrrsTags = Lens.field @"tags"
{-# DEPRECATED ltfvrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvrrsResponseStatus :: Lens.Lens' ListTagsForVaultResponse Core.Int
ltfvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
