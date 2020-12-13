{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListRulesPackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available Amazon Inspector rules packages.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListRulesPackages
  ( -- * Creating a request
    ListRulesPackages (..),
    mkListRulesPackages,

    -- ** Request lenses
    lrpNextToken,
    lrpMaxResults,

    -- * Destructuring the response
    ListRulesPackagesResponse (..),
    mkListRulesPackagesResponse,

    -- ** Response lenses
    lrprsRulesPackageARNs,
    lrprsNextToken,
    lrprsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRulesPackages' smart constructor.
data ListRulesPackages = ListRulesPackages'
  { -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListRulesPackages__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRulesPackages' with the minimum fields required to make a request.
--
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListRulesPackages__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
mkListRulesPackages ::
  ListRulesPackages
mkListRulesPackages =
  ListRulesPackages'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListRulesPackages__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpNextToken :: Lens.Lens' ListRulesPackages (Lude.Maybe Lude.Text)
lrpNextToken = Lens.lens (nextToken :: ListRulesPackages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRulesPackages)
{-# DEPRECATED lrpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMaxResults :: Lens.Lens' ListRulesPackages (Lude.Maybe Lude.Int)
lrpMaxResults = Lens.lens (maxResults :: ListRulesPackages -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListRulesPackages)
{-# DEPRECATED lrpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListRulesPackages where
  page rq rs
    | Page.stop (rs Lens.^. lrprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrprsRulesPackageARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrpNextToken Lens..~ rs Lens.^. lrprsNextToken

instance Lude.AWSRequest ListRulesPackages where
  type Rs ListRulesPackages = ListRulesPackagesResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRulesPackagesResponse'
            Lude.<$> (x Lude..?> "rulesPackageArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRulesPackages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.ListRulesPackages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRulesPackages where
  toJSON ListRulesPackages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListRulesPackages where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRulesPackages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRulesPackagesResponse' smart constructor.
data ListRulesPackagesResponse = ListRulesPackagesResponse'
  { -- | The list of ARNs that specifies the rules packages returned by the action.
    rulesPackageARNs :: [Lude.Text],
    -- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRulesPackagesResponse' with the minimum fields required to make a request.
--
-- * 'rulesPackageARNs' - The list of ARNs that specifies the rules packages returned by the action.
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
mkListRulesPackagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRulesPackagesResponse
mkListRulesPackagesResponse pResponseStatus_ =
  ListRulesPackagesResponse'
    { rulesPackageARNs = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of ARNs that specifies the rules packages returned by the action.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsRulesPackageARNs :: Lens.Lens' ListRulesPackagesResponse [Lude.Text]
lrprsRulesPackageARNs = Lens.lens (rulesPackageARNs :: ListRulesPackagesResponse -> [Lude.Text]) (\s a -> s {rulesPackageARNs = a} :: ListRulesPackagesResponse)
{-# DEPRECATED lrprsRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsNextToken :: Lens.Lens' ListRulesPackagesResponse (Lude.Maybe Lude.Text)
lrprsNextToken = Lens.lens (nextToken :: ListRulesPackagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRulesPackagesResponse)
{-# DEPRECATED lrprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsResponseStatus :: Lens.Lens' ListRulesPackagesResponse Lude.Int
lrprsResponseStatus = Lens.lens (responseStatus :: ListRulesPackagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRulesPackagesResponse)
{-# DEPRECATED lrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
