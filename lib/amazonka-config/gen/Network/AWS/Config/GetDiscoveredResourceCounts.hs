{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetDiscoveredResourceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource types, the number of each resource type, and the total number of resources that AWS Config is recording in this region for your AWS account.
--
-- __Example__
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets.
--
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify that you want all resource types.
--
--
--     * AWS Config returns the following:
--
--     * The resource types (EC2 instances, IAM users, and S3 buckets).
--
--
--     * The number of each resource type (25, 20, and 15).
--
--
--     * The total number of all resources (60).
--
--
--
--
-- The response is paginated. By default, AWS Config lists 100 'ResourceCount' objects on each page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
module Network.AWS.Config.GetDiscoveredResourceCounts
  ( -- * Creating a request
    GetDiscoveredResourceCounts (..),
    mkGetDiscoveredResourceCounts,

    -- ** Request lenses
    gdrcNextToken,
    gdrcLimit,
    gdrcResourceTypes,

    -- * Destructuring the response
    GetDiscoveredResourceCountsResponse (..),
    mkGetDiscoveredResourceCountsResponse,

    -- ** Response lenses
    gdrcrsTotalDiscoveredResources,
    gdrcrsNextToken,
    gdrcrsResourceCounts,
    gdrcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDiscoveredResourceCounts' smart constructor.
data GetDiscoveredResourceCounts = GetDiscoveredResourceCounts'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    resourceTypes ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiscoveredResourceCounts' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of 'ResourceCount' objects returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'resourceTypes' - The comma-separated list that specifies the resource types that you want AWS Config to return (for example, @"AWS::EC2::Instance"@ , @"AWS::IAM::User"@ ).
--
-- If a value for @resourceTypes@ is not specified, AWS Config returns all resource types that AWS Config is recording in the region for your account.
mkGetDiscoveredResourceCounts ::
  GetDiscoveredResourceCounts
mkGetDiscoveredResourceCounts =
  GetDiscoveredResourceCounts'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      resourceTypes = Lude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcNextToken :: Lens.Lens' GetDiscoveredResourceCounts (Lude.Maybe Lude.Text)
gdrcNextToken = Lens.lens (nextToken :: GetDiscoveredResourceCounts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDiscoveredResourceCounts)
{-# DEPRECATED gdrcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of 'ResourceCount' objects returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcLimit :: Lens.Lens' GetDiscoveredResourceCounts (Lude.Maybe Lude.Natural)
gdrcLimit = Lens.lens (limit :: GetDiscoveredResourceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetDiscoveredResourceCounts)
{-# DEPRECATED gdrcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The comma-separated list that specifies the resource types that you want AWS Config to return (for example, @"AWS::EC2::Instance"@ , @"AWS::IAM::User"@ ).
--
-- If a value for @resourceTypes@ is not specified, AWS Config returns all resource types that AWS Config is recording in the region for your account.
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcResourceTypes :: Lens.Lens' GetDiscoveredResourceCounts (Lude.Maybe [Lude.Text])
gdrcResourceTypes = Lens.lens (resourceTypes :: GetDiscoveredResourceCounts -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: GetDiscoveredResourceCounts)
{-# DEPRECATED gdrcResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

instance Lude.AWSRequest GetDiscoveredResourceCounts where
  type
    Rs GetDiscoveredResourceCounts =
      GetDiscoveredResourceCountsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDiscoveredResourceCountsResponse'
            Lude.<$> (x Lude..?> "totalDiscoveredResources")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "resourceCounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDiscoveredResourceCounts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetDiscoveredResourceCounts" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDiscoveredResourceCounts where
  toJSON GetDiscoveredResourceCounts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit,
            ("resourceTypes" Lude..=) Lude.<$> resourceTypes
          ]
      )

instance Lude.ToPath GetDiscoveredResourceCounts where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDiscoveredResourceCounts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDiscoveredResourceCountsResponse' smart constructor.
data GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse'
  { totalDiscoveredResources ::
      Lude.Maybe
        Lude.Integer,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    resourceCounts ::
      Lude.Maybe
        [ResourceCount],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiscoveredResourceCountsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'resourceCounts' - The list of @ResourceCount@ objects. Each object is listed in descending order by the number of resources.
-- * 'responseStatus' - The response status code.
-- * 'totalDiscoveredResources' - The total number of resources that AWS Config is recording in the region for your account. If you specify resource types in the request, AWS Config returns only the total number of resources for those resource types.
--
-- __Example__
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets, for a total of 60 resources.
--
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify the resource type, @"AWS::EC2::Instances"@ , in the request.
--
--
--     * AWS Config returns 25 for @totalDiscoveredResources@ .
mkGetDiscoveredResourceCountsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDiscoveredResourceCountsResponse
mkGetDiscoveredResourceCountsResponse pResponseStatus_ =
  GetDiscoveredResourceCountsResponse'
    { totalDiscoveredResources =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      resourceCounts = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The total number of resources that AWS Config is recording in the region for your account. If you specify resource types in the request, AWS Config returns only the total number of resources for those resource types.
--
-- __Example__
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets, for a total of 60 resources.
--
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify the resource type, @"AWS::EC2::Instances"@ , in the request.
--
--
--     * AWS Config returns 25 for @totalDiscoveredResources@ .
--
--
--
-- /Note:/ Consider using 'totalDiscoveredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrsTotalDiscoveredResources :: Lens.Lens' GetDiscoveredResourceCountsResponse (Lude.Maybe Lude.Integer)
gdrcrsTotalDiscoveredResources = Lens.lens (totalDiscoveredResources :: GetDiscoveredResourceCountsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {totalDiscoveredResources = a} :: GetDiscoveredResourceCountsResponse)
{-# DEPRECATED gdrcrsTotalDiscoveredResources "Use generic-lens or generic-optics with 'totalDiscoveredResources' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrsNextToken :: Lens.Lens' GetDiscoveredResourceCountsResponse (Lude.Maybe Lude.Text)
gdrcrsNextToken = Lens.lens (nextToken :: GetDiscoveredResourceCountsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDiscoveredResourceCountsResponse)
{-# DEPRECATED gdrcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of @ResourceCount@ objects. Each object is listed in descending order by the number of resources.
--
-- /Note:/ Consider using 'resourceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrsResourceCounts :: Lens.Lens' GetDiscoveredResourceCountsResponse (Lude.Maybe [ResourceCount])
gdrcrsResourceCounts = Lens.lens (resourceCounts :: GetDiscoveredResourceCountsResponse -> Lude.Maybe [ResourceCount]) (\s a -> s {resourceCounts = a} :: GetDiscoveredResourceCountsResponse)
{-# DEPRECATED gdrcrsResourceCounts "Use generic-lens or generic-optics with 'resourceCounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrsResponseStatus :: Lens.Lens' GetDiscoveredResourceCountsResponse Lude.Int
gdrcrsResponseStatus = Lens.lens (responseStatus :: GetDiscoveredResourceCountsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDiscoveredResourceCountsResponse)
{-# DEPRECATED gdrcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
