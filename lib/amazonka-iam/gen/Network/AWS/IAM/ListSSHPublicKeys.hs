{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lsshpkMarker,
    lsshpkMaxItems,
    lsshpkUserName,

    -- * Destructuring the response
    ListSSHPublicKeysResponse (..),
    mkListSSHPublicKeysResponse,

    -- ** Response lenses
    lsshpkrrsIsTruncated,
    lsshpkrrsMarker,
    lsshpkrrsSSHPublicKeys,
    lsshpkrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSSHPublicKeys' smart constructor.
data ListSSHPublicKeys = ListSSHPublicKeys'
  { -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | The name of the IAM user to list SSH public keys for. If none is specified, the @UserName@ field is determined implicitly based on the AWS access key used to sign the request.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSSHPublicKeys' value with any optional fields omitted.
mkListSSHPublicKeys ::
  ListSSHPublicKeys
mkListSSHPublicKeys =
  ListSSHPublicKeys'
    { marker = Core.Nothing,
      maxItems = Core.Nothing,
      userName = Core.Nothing
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkMarker :: Lens.Lens' ListSSHPublicKeys (Core.Maybe Types.MarkerType)
lsshpkMarker = Lens.field @"marker"
{-# DEPRECATED lsshpkMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkMaxItems :: Lens.Lens' ListSSHPublicKeys (Core.Maybe Core.Natural)
lsshpkMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lsshpkMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the IAM user to list SSH public keys for. If none is specified, the @UserName@ field is determined implicitly based on the AWS access key used to sign the request.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkUserName :: Lens.Lens' ListSSHPublicKeys (Core.Maybe Types.UserNameType)
lsshpkUserName = Lens.field @"userName"
{-# DEPRECATED lsshpkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest ListSSHPublicKeys where
  type Rs ListSSHPublicKeys = ListSSHPublicKeysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListSSHPublicKeys")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListSSHPublicKeysResult"
      ( \s h x ->
          ListSSHPublicKeysResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (x Core..@? "SSHPublicKeys" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSSHPublicKeys where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListSSHPublicKeys' request.
--
-- /See:/ 'mkListSSHPublicKeysResponse' smart constructor.
data ListSSHPublicKeysResponse = ListSSHPublicKeysResponse'
  { -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.Marker,
    -- | A list of the SSH public keys assigned to IAM user.
    sSHPublicKeys :: Core.Maybe [Types.SSHPublicKeyMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSSHPublicKeysResponse' value with any optional fields omitted.
mkListSSHPublicKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSSHPublicKeysResponse
mkListSSHPublicKeysResponse responseStatus =
  ListSSHPublicKeysResponse'
    { isTruncated = Core.Nothing,
      marker = Core.Nothing,
      sSHPublicKeys = Core.Nothing,
      responseStatus
    }

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkrrsIsTruncated :: Lens.Lens' ListSSHPublicKeysResponse (Core.Maybe Core.Bool)
lsshpkrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lsshpkrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkrrsMarker :: Lens.Lens' ListSSHPublicKeysResponse (Core.Maybe Types.Marker)
lsshpkrrsMarker = Lens.field @"marker"
{-# DEPRECATED lsshpkrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of the SSH public keys assigned to IAM user.
--
-- /Note:/ Consider using 'sSHPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkrrsSSHPublicKeys :: Lens.Lens' ListSSHPublicKeysResponse (Core.Maybe [Types.SSHPublicKeyMetadata])
lsshpkrrsSSHPublicKeys = Lens.field @"sSHPublicKeys"
{-# DEPRECATED lsshpkrrsSSHPublicKeys "Use generic-lens or generic-optics with 'sSHPublicKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsshpkrrsResponseStatus :: Lens.Lens' ListSSHPublicKeysResponse Core.Int
lsshpkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsshpkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
