{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSSHPublicKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the SSH public keys associated with the specified IAM user. If none exists, the operation returns an empty list.
--
-- The SSH public keys returned by this operation are used only for authenticating the IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
-- Although each user is limited to a small number of keys, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListSSHPublicKeys
  ( -- * Creating a request
    ListSSHPublicKeys (..),
    mkListSSHPublicKeys,

    -- ** Request lenses
    lspkUserName,
    lspkMarker,
    lspkMaxItems,

    -- * Destructuring the response
    ListSSHPublicKeysResponse (..),
    mkListSSHPublicKeysResponse,

    -- ** Response lenses
    lspkrsSSHPublicKeys,
    lspkrsMarker,
    lspkrsIsTruncated,
    lspkrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSSHPublicKeys' smart constructor.
data ListSSHPublicKeys = ListSSHPublicKeys'
  { userName ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSSHPublicKeys' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'userName' - The name of the IAM user to list SSH public keys for. If none is specified, the @UserName@ field is determined implicitly based on the AWS access key used to sign the request.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkListSSHPublicKeys ::
  ListSSHPublicKeys
mkListSSHPublicKeys =
  ListSSHPublicKeys'
    { userName = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the IAM user to list SSH public keys for. If none is specified, the @UserName@ field is determined implicitly based on the AWS access key used to sign the request.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkUserName :: Lens.Lens' ListSSHPublicKeys (Lude.Maybe Lude.Text)
lspkUserName = Lens.lens (userName :: ListSSHPublicKeys -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: ListSSHPublicKeys)
{-# DEPRECATED lspkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkMarker :: Lens.Lens' ListSSHPublicKeys (Lude.Maybe Lude.Text)
lspkMarker = Lens.lens (marker :: ListSSHPublicKeys -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListSSHPublicKeys)
{-# DEPRECATED lspkMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkMaxItems :: Lens.Lens' ListSSHPublicKeys (Lude.Maybe Lude.Natural)
lspkMaxItems = Lens.lens (maxItems :: ListSSHPublicKeys -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListSSHPublicKeys)
{-# DEPRECATED lspkMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListSSHPublicKeys where
  page rq rs
    | Page.stop (rs Lens.^. lspkrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lspkrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lspkMarker Lens..~ rs Lens.^. lspkrsMarker

instance Lude.AWSRequest ListSSHPublicKeys where
  type Rs ListSSHPublicKeys = ListSSHPublicKeysResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListSSHPublicKeysResult"
      ( \s h x ->
          ListSSHPublicKeysResponse'
            Lude.<$> ( x Lude..@? "SSHPublicKeys" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSSHPublicKeys where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSSHPublicKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSSHPublicKeys where
  toQuery ListSSHPublicKeys' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListSSHPublicKeys" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListSSHPublicKeys' request.
--
-- /See:/ 'mkListSSHPublicKeysResponse' smart constructor.
data ListSSHPublicKeysResponse = ListSSHPublicKeysResponse'
  { sshPublicKeys ::
      Lude.Maybe [SSHPublicKeyMetadata],
    marker :: Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSSHPublicKeysResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
-- * 'sshPublicKeys' - A list of the SSH public keys assigned to IAM user.
mkListSSHPublicKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSSHPublicKeysResponse
mkListSSHPublicKeysResponse pResponseStatus_ =
  ListSSHPublicKeysResponse'
    { sshPublicKeys = Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the SSH public keys assigned to IAM user.
--
-- /Note:/ Consider using 'sshPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkrsSSHPublicKeys :: Lens.Lens' ListSSHPublicKeysResponse (Lude.Maybe [SSHPublicKeyMetadata])
lspkrsSSHPublicKeys = Lens.lens (sshPublicKeys :: ListSSHPublicKeysResponse -> Lude.Maybe [SSHPublicKeyMetadata]) (\s a -> s {sshPublicKeys = a} :: ListSSHPublicKeysResponse)
{-# DEPRECATED lspkrsSSHPublicKeys "Use generic-lens or generic-optics with 'sshPublicKeys' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkrsMarker :: Lens.Lens' ListSSHPublicKeysResponse (Lude.Maybe Lude.Text)
lspkrsMarker = Lens.lens (marker :: ListSSHPublicKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListSSHPublicKeysResponse)
{-# DEPRECATED lspkrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkrsIsTruncated :: Lens.Lens' ListSSHPublicKeysResponse (Lude.Maybe Lude.Bool)
lspkrsIsTruncated = Lens.lens (isTruncated :: ListSSHPublicKeysResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListSSHPublicKeysResponse)
{-# DEPRECATED lspkrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspkrsResponseStatus :: Lens.Lens' ListSSHPublicKeysResponse Lude.Int
lspkrsResponseStatus = Lens.lens (responseStatus :: ListSSHPublicKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSSHPublicKeysResponse)
{-# DEPRECATED lspkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
