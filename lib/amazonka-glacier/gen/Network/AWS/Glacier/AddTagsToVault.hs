{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.AddTagsToVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds the specified tags to a vault. Each tag is composed of a key and a value. Each vault can have up to 10 tags. If your request would cause the tag limit for the vault to be exceeded, the operation throws the @LimitExceededException@ error. If a tag already exists on the vault under a specified key, the existing key value will be overwritten. For more information about tags, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon S3 Glacier Resources> .
module Network.AWS.Glacier.AddTagsToVault
  ( -- * Creating a request
    AddTagsToVault (..),
    mkAddTagsToVault,

    -- ** Request lenses
    attvTags,
    attvAccountId,
    attvVaultName,

    -- * Destructuring the response
    AddTagsToVaultResponse (..),
    mkAddTagsToVaultResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input values for @AddTagsToVault@ .
--
-- /See:/ 'mkAddTagsToVault' smart constructor.
data AddTagsToVault = AddTagsToVault'
  { tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    accountId :: Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToVault' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'tags' - The tags to add to the vault. Each tag is composed of a key and a value. The value can be an empty string.
-- * 'vaultName' - The name of the vault.
mkAddTagsToVault ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  AddTagsToVault
mkAddTagsToVault pAccountId_ pVaultName_ =
  AddTagsToVault'
    { tags = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The tags to add to the vault. Each tag is composed of a key and a value. The value can be an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attvTags :: Lens.Lens' AddTagsToVault (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
attvTags = Lens.lens (tags :: AddTagsToVault -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: AddTagsToVault)
{-# DEPRECATED attvTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attvAccountId :: Lens.Lens' AddTagsToVault Lude.Text
attvAccountId = Lens.lens (accountId :: AddTagsToVault -> Lude.Text) (\s a -> s {accountId = a} :: AddTagsToVault)
{-# DEPRECATED attvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attvVaultName :: Lens.Lens' AddTagsToVault Lude.Text
attvVaultName = Lens.lens (vaultName :: AddTagsToVault -> Lude.Text) (\s a -> s {vaultName = a} :: AddTagsToVault)
{-# DEPRECATED attvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest AddTagsToVault where
  type Rs AddTagsToVault = AddTagsToVaultResponse
  request = Req.postJSON glacierService
  response = Res.receiveNull AddTagsToVaultResponse'

instance Lude.ToHeaders AddTagsToVault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AddTagsToVault where
  toJSON AddTagsToVault' {..} =
    Lude.object (Lude.catMaybes [("Tags" Lude..=) Lude.<$> tags])

instance Lude.ToPath AddTagsToVault where
  toPath AddTagsToVault' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/tags"
      ]

instance Lude.ToQuery AddTagsToVault where
  toQuery = Lude.const (Lude.mconcat ["operation=add"])

-- | /See:/ 'mkAddTagsToVaultResponse' smart constructor.
data AddTagsToVaultResponse = AddTagsToVaultResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToVaultResponse' with the minimum fields required to make a request.
mkAddTagsToVaultResponse ::
  AddTagsToVaultResponse
mkAddTagsToVaultResponse = AddTagsToVaultResponse'
