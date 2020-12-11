{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account alias associated with the AWS account (Note: you can have only one). For information about using an AWS account alias, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccountAliases
  ( -- * Creating a request
    ListAccountAliases (..),
    mkListAccountAliases,

    -- ** Request lenses
    laaMarker,
    laaMaxItems,

    -- * Destructuring the response
    ListAccountAliasesResponse (..),
    mkListAccountAliasesResponse,

    -- ** Response lenses
    laarsMarker,
    laarsIsTruncated,
    laarsResponseStatus,
    laarsAccountAliases,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAccountAliases' smart constructor.
data ListAccountAliases = ListAccountAliases'
  { marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListAccountAliases' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListAccountAliases ::
  ListAccountAliases
mkListAccountAliases =
  ListAccountAliases'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaMarker :: Lens.Lens' ListAccountAliases (Lude.Maybe Lude.Text)
laaMarker = Lens.lens (marker :: ListAccountAliases -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAccountAliases)
{-# DEPRECATED laaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaMaxItems :: Lens.Lens' ListAccountAliases (Lude.Maybe Lude.Natural)
laaMaxItems = Lens.lens (maxItems :: ListAccountAliases -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListAccountAliases)
{-# DEPRECATED laaMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListAccountAliases where
  page rq rs
    | Page.stop (rs Lens.^. laarsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. laarsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& laaMarker Lens..~ rs Lens.^. laarsMarker

instance Lude.AWSRequest ListAccountAliases where
  type Rs ListAccountAliases = ListAccountAliasesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListAccountAliasesResult"
      ( \s h x ->
          ListAccountAliasesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "AccountAliases" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListAccountAliases where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAccountAliases where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAccountAliases where
  toQuery ListAccountAliases' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListAccountAliases" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListAccountAliases' request.
--
-- /See:/ 'mkListAccountAliasesResponse' smart constructor.
data ListAccountAliasesResponse = ListAccountAliasesResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    accountAliases :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccountAliasesResponse' with the minimum fields required to make a request.
--
-- * 'accountAliases' - A list of aliases associated with the account. AWS supports only one alias per account.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
mkListAccountAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAccountAliasesResponse
mkListAccountAliasesResponse pResponseStatus_ =
  ListAccountAliasesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      accountAliases = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarsMarker :: Lens.Lens' ListAccountAliasesResponse (Lude.Maybe Lude.Text)
laarsMarker = Lens.lens (marker :: ListAccountAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAccountAliasesResponse)
{-# DEPRECATED laarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarsIsTruncated :: Lens.Lens' ListAccountAliasesResponse (Lude.Maybe Lude.Bool)
laarsIsTruncated = Lens.lens (isTruncated :: ListAccountAliasesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListAccountAliasesResponse)
{-# DEPRECATED laarsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarsResponseStatus :: Lens.Lens' ListAccountAliasesResponse Lude.Int
laarsResponseStatus = Lens.lens (responseStatus :: ListAccountAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAccountAliasesResponse)
{-# DEPRECATED laarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of aliases associated with the account. AWS supports only one alias per account.
--
-- /Note:/ Consider using 'accountAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarsAccountAliases :: Lens.Lens' ListAccountAliasesResponse [Lude.Text]
laarsAccountAliases = Lens.lens (accountAliases :: ListAccountAliasesResponse -> [Lude.Text]) (\s a -> s {accountAliases = a} :: ListAccountAliasesResponse)
{-# DEPRECATED laarsAccountAliases "Use generic-lens or generic-optics with 'accountAliases' instead." #-}
