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
    ltfvVaultName,
    ltfvAccountId,

    -- * Destructuring the response
    ListTagsForVaultResponse (..),
    mkListTagsForVaultResponse,

    -- ** Response lenses
    ltfvrsTags,
    ltfvrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input value for @ListTagsForVaultInput@ .
--
-- /See:/ 'mkListTagsForVault' smart constructor.
data ListTagsForVault = ListTagsForVault'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForVault' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
mkListTagsForVault ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  ListTagsForVault
mkListTagsForVault pVaultName_ pAccountId_ =
  ListTagsForVault'
    { vaultName = pVaultName_,
      accountId = pAccountId_
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvVaultName :: Lens.Lens' ListTagsForVault Lude.Text
ltfvVaultName = Lens.lens (vaultName :: ListTagsForVault -> Lude.Text) (\s a -> s {vaultName = a} :: ListTagsForVault)
{-# DEPRECATED ltfvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvAccountId :: Lens.Lens' ListTagsForVault Lude.Text
ltfvAccountId = Lens.lens (accountId :: ListTagsForVault -> Lude.Text) (\s a -> s {accountId = a} :: ListTagsForVault)
{-# DEPRECATED ltfvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest ListTagsForVault where
  type Rs ListTagsForVault = ListTagsForVaultResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForVaultResponse'
            Lude.<$> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForVault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForVault where
  toPath ListTagsForVault' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/tags"
      ]

instance Lude.ToQuery ListTagsForVault where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListTagsForVaultResponse' smart constructor.
data ListTagsForVaultResponse = ListTagsForVaultResponse'
  { -- | The tags attached to the vault. Each tag is composed of a key and a value.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForVaultResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The tags attached to the vault. Each tag is composed of a key and a value.
-- * 'responseStatus' - The response status code.
mkListTagsForVaultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForVaultResponse
mkListTagsForVaultResponse pResponseStatus_ =
  ListTagsForVaultResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The tags attached to the vault. Each tag is composed of a key and a value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvrsTags :: Lens.Lens' ListTagsForVaultResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ltfvrsTags = Lens.lens (tags :: ListTagsForVaultResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListTagsForVaultResponse)
{-# DEPRECATED ltfvrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfvrsResponseStatus :: Lens.Lens' ListTagsForVaultResponse Lude.Int
ltfvrsResponseStatus = Lens.lens (responseStatus :: ListTagsForVaultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForVaultResponse)
{-# DEPRECATED ltfvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
