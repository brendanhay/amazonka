{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListVaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all vaults owned by the calling user's account. The list returned in the response is ASCII-sorted by vault name.
--
-- By default, this operation returns up to 10 items. If there are more vaults to list, the response @marker@ field contains the vault Amazon Resource Name (ARN) at which to continue the list with a new List Vaults request; otherwise, the @marker@ field is @null@ . To return a list of vaults that begins at a specific vault, set the @marker@ request parameter to the vault ARN you obtained from a previous List Vaults request. You can also limit the number of vaults returned in the response by specifying the @limit@ parameter in the request.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html List Vaults > in the /Amazon Glacier Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListVaults
  ( -- * Creating a request
    ListVaults (..),
    mkListVaults,

    -- ** Request lenses
    lvAccountId,
    lvMarker,
    lvLimit,

    -- * Destructuring the response
    ListVaultsResponse (..),
    mkListVaultsResponse,

    -- ** Response lenses
    lvrsMarker,
    lvrsVaultList,
    lvrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to retrieve the vault list owned by the calling user's account. The list provides metadata information for each vault.
--
-- /See:/ 'mkListVaults' smart constructor.
data ListVaults = ListVaults'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text,
    -- | A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of vaults to be returned. The default limit is 10. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
    limit :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVaults' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
-- * 'marker' - A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
-- * 'limit' - The maximum number of vaults to be returned. The default limit is 10. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
mkListVaults ::
  -- | 'accountId'
  Lude.Text ->
  ListVaults
mkListVaults pAccountId_ =
  ListVaults'
    { accountId = pAccountId_,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvAccountId :: Lens.Lens' ListVaults Lude.Text
lvAccountId = Lens.lens (accountId :: ListVaults -> Lude.Text) (\s a -> s {accountId = a} :: ListVaults)
{-# DEPRECATED lvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvMarker :: Lens.Lens' ListVaults (Lude.Maybe Lude.Text)
lvMarker = Lens.lens (marker :: ListVaults -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVaults)
{-# DEPRECATED lvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of vaults to be returned. The default limit is 10. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvLimit :: Lens.Lens' ListVaults (Lude.Maybe Lude.Text)
lvLimit = Lens.lens (limit :: ListVaults -> Lude.Maybe Lude.Text) (\s a -> s {limit = a} :: ListVaults)
{-# DEPRECATED lvLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListVaults where
  page rq rs
    | Page.stop (rs Lens.^. lvrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lvrsVaultList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lvMarker Lens..~ rs Lens.^. lvrsMarker

instance Lude.AWSRequest ListVaults where
  type Rs ListVaults = ListVaultsResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVaultsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "VaultList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVaults where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListVaults where
  toPath ListVaults' {..} =
    Lude.mconcat ["/", Lude.toBS accountId, "/vaults"]

instance Lude.ToQuery ListVaults where
  toQuery ListVaults' {..} =
    Lude.mconcat ["marker" Lude.=: marker, "limit" Lude.=: limit]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListVaultsResponse' smart constructor.
data ListVaultsResponse = ListVaultsResponse'
  { -- | The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
    marker :: Lude.Maybe Lude.Text,
    -- | List of vaults.
    vaultList :: Lude.Maybe [DescribeVaultOutput],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVaultsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
-- * 'vaultList' - List of vaults.
-- * 'responseStatus' - The response status code.
mkListVaultsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVaultsResponse
mkListVaultsResponse pResponseStatus_ =
  ListVaultsResponse'
    { marker = Lude.Nothing,
      vaultList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsMarker :: Lens.Lens' ListVaultsResponse (Lude.Maybe Lude.Text)
lvrsMarker = Lens.lens (marker :: ListVaultsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVaultsResponse)
{-# DEPRECATED lvrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | List of vaults.
--
-- /Note:/ Consider using 'vaultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsVaultList :: Lens.Lens' ListVaultsResponse (Lude.Maybe [DescribeVaultOutput])
lvrsVaultList = Lens.lens (vaultList :: ListVaultsResponse -> Lude.Maybe [DescribeVaultOutput]) (\s a -> s {vaultList = a} :: ListVaultsResponse)
{-# DEPRECATED lvrsVaultList "Use generic-lens or generic-optics with 'vaultList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsResponseStatus :: Lens.Lens' ListVaultsResponse Lude.Int
lvrsResponseStatus = Lens.lens (responseStatus :: ListVaultsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVaultsResponse)
{-# DEPRECATED lvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
