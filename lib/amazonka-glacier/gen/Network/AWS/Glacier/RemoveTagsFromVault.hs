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
    rtfvVaultName,
    rtfvTagKeys,
    rtfvAccountId,

    -- * Destructuring the response
    RemoveTagsFromVaultResponse (..),
    mkRemoveTagsFromVaultResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input value for @RemoveTagsFromVaultInput@ .
--
-- /See:/ 'mkRemoveTagsFromVault' smart constructor.
data RemoveTagsFromVault = RemoveTagsFromVault'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | A list of tag keys. Each corresponding tag is removed from the vault.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromVault' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'tagKeys' - A list of tag keys. Each corresponding tag is removed from the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
mkRemoveTagsFromVault ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  RemoveTagsFromVault
mkRemoveTagsFromVault pVaultName_ pAccountId_ =
  RemoveTagsFromVault'
    { vaultName = pVaultName_,
      tagKeys = Lude.Nothing,
      accountId = pAccountId_
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvVaultName :: Lens.Lens' RemoveTagsFromVault Lude.Text
rtfvVaultName = Lens.lens (vaultName :: RemoveTagsFromVault -> Lude.Text) (\s a -> s {vaultName = a} :: RemoveTagsFromVault)
{-# DEPRECATED rtfvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | A list of tag keys. Each corresponding tag is removed from the vault.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvTagKeys :: Lens.Lens' RemoveTagsFromVault (Lude.Maybe [Lude.Text])
rtfvTagKeys = Lens.lens (tagKeys :: RemoveTagsFromVault -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTagsFromVault)
{-# DEPRECATED rtfvTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfvAccountId :: Lens.Lens' RemoveTagsFromVault Lude.Text
rtfvAccountId = Lens.lens (accountId :: RemoveTagsFromVault -> Lude.Text) (\s a -> s {accountId = a} :: RemoveTagsFromVault)
{-# DEPRECATED rtfvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest RemoveTagsFromVault where
  type Rs RemoveTagsFromVault = RemoveTagsFromVaultResponse
  request = Req.postJSON glacierService
  response = Res.receiveNull RemoveTagsFromVaultResponse'

instance Lude.ToHeaders RemoveTagsFromVault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RemoveTagsFromVault where
  toJSON RemoveTagsFromVault' {..} =
    Lude.object
      (Lude.catMaybes [("TagKeys" Lude..=) Lude.<$> tagKeys])

instance Lude.ToPath RemoveTagsFromVault where
  toPath RemoveTagsFromVault' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/tags"
      ]

instance Lude.ToQuery RemoveTagsFromVault where
  toQuery = Lude.const (Lude.mconcat ["operation=remove"])

-- | /See:/ 'mkRemoveTagsFromVaultResponse' smart constructor.
data RemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromVaultResponse' with the minimum fields required to make a request.
mkRemoveTagsFromVaultResponse ::
  RemoveTagsFromVaultResponse
mkRemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
