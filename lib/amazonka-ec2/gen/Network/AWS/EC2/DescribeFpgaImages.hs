{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFpgaImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Amazon FPGA Images (AFIs) available to you. These include public AFIs, private AFIs that you own, and AFIs owned by other AWS accounts for which you have load permissions.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFpgaImages
  ( -- * Creating a request
    DescribeFpgaImages (..),
    mkDescribeFpgaImages,

    -- ** Request lenses
    dfisOwners,
    dfisFilters,
    dfisNextToken,
    dfisDryRun,
    dfisMaxResults,
    dfisFpgaImageIds,

    -- * Destructuring the response
    DescribeFpgaImagesResponse (..),
    mkDescribeFpgaImagesResponse,

    -- ** Response lenses
    dfirsFpgaImages,
    dfirsNextToken,
    dfirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFpgaImages' smart constructor.
data DescribeFpgaImages = DescribeFpgaImages'
  { -- | Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
    owners :: Lude.Maybe [Lude.Text],
    -- | The filters.
    --
    --
    --     * @create-time@ - The creation time of the AFI.
    --
    --
    --     * @fpga-image-id@ - The FPGA image identifier (AFI ID).
    --
    --
    --     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).
    --
    --
    --     * @name@ - The name of the AFI.
    --
    --
    --     * @owner-id@ - The AWS account ID of the AFI owner.
    --
    --
    --     * @product-code@ - The product code.
    --
    --
    --     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.
    --
    --
    --     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @update-time@ - The time of the most recent update.
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The AFI IDs.
    fpgaImageIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFpgaImages' with the minimum fields required to make a request.
--
-- * 'owners' - Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
-- * 'filters' - The filters.
--
--
--     * @create-time@ - The creation time of the AFI.
--
--
--     * @fpga-image-id@ - The FPGA image identifier (AFI ID).
--
--
--     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).
--
--
--     * @name@ - The name of the AFI.
--
--
--     * @owner-id@ - The AWS account ID of the AFI owner.
--
--
--     * @product-code@ - The product code.
--
--
--     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.
--
--
--     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @update-time@ - The time of the most recent update.
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'fpgaImageIds' - The AFI IDs.
mkDescribeFpgaImages ::
  DescribeFpgaImages
mkDescribeFpgaImages =
  DescribeFpgaImages'
    { owners = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      fpgaImageIds = Lude.Nothing
    }

-- | Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
--
-- /Note:/ Consider using 'owners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisOwners :: Lens.Lens' DescribeFpgaImages (Lude.Maybe [Lude.Text])
dfisOwners = Lens.lens (owners :: DescribeFpgaImages -> Lude.Maybe [Lude.Text]) (\s a -> s {owners = a} :: DescribeFpgaImages)
{-# DEPRECATED dfisOwners "Use generic-lens or generic-optics with 'owners' instead." #-}

-- | The filters.
--
--
--     * @create-time@ - The creation time of the AFI.
--
--
--     * @fpga-image-id@ - The FPGA image identifier (AFI ID).
--
--
--     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).
--
--
--     * @name@ - The name of the AFI.
--
--
--     * @owner-id@ - The AWS account ID of the AFI owner.
--
--
--     * @product-code@ - The product code.
--
--
--     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.
--
--
--     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @update-time@ - The time of the most recent update.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisFilters :: Lens.Lens' DescribeFpgaImages (Lude.Maybe [Filter])
dfisFilters = Lens.lens (filters :: DescribeFpgaImages -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeFpgaImages)
{-# DEPRECATED dfisFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisNextToken :: Lens.Lens' DescribeFpgaImages (Lude.Maybe Lude.Text)
dfisNextToken = Lens.lens (nextToken :: DescribeFpgaImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFpgaImages)
{-# DEPRECATED dfisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisDryRun :: Lens.Lens' DescribeFpgaImages (Lude.Maybe Lude.Bool)
dfisDryRun = Lens.lens (dryRun :: DescribeFpgaImages -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFpgaImages)
{-# DEPRECATED dfisDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisMaxResults :: Lens.Lens' DescribeFpgaImages (Lude.Maybe Lude.Natural)
dfisMaxResults = Lens.lens (maxResults :: DescribeFpgaImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeFpgaImages)
{-# DEPRECATED dfisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The AFI IDs.
--
-- /Note:/ Consider using 'fpgaImageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisFpgaImageIds :: Lens.Lens' DescribeFpgaImages (Lude.Maybe [Lude.Text])
dfisFpgaImageIds = Lens.lens (fpgaImageIds :: DescribeFpgaImages -> Lude.Maybe [Lude.Text]) (\s a -> s {fpgaImageIds = a} :: DescribeFpgaImages)
{-# DEPRECATED dfisFpgaImageIds "Use generic-lens or generic-optics with 'fpgaImageIds' instead." #-}

instance Page.AWSPager DescribeFpgaImages where
  page rq rs
    | Page.stop (rs Lens.^. dfirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfirsFpgaImages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfisNextToken Lens..~ rs Lens.^. dfirsNextToken

instance Lude.AWSRequest DescribeFpgaImages where
  type Rs DescribeFpgaImages = DescribeFpgaImagesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFpgaImagesResponse'
            Lude.<$> ( x Lude..@? "fpgaImageSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFpgaImages where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFpgaImages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFpgaImages where
  toQuery DescribeFpgaImages' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeFpgaImages" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Owner" Lude.<$> owners),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        Lude.toQuery
          (Lude.toQueryList "FpgaImageId" Lude.<$> fpgaImageIds)
      ]

-- | /See:/ 'mkDescribeFpgaImagesResponse' smart constructor.
data DescribeFpgaImagesResponse = DescribeFpgaImagesResponse'
  { -- | Information about the FPGA images.
    fpgaImages :: Lude.Maybe [FpgaImage],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFpgaImagesResponse' with the minimum fields required to make a request.
--
-- * 'fpgaImages' - Information about the FPGA images.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeFpgaImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFpgaImagesResponse
mkDescribeFpgaImagesResponse pResponseStatus_ =
  DescribeFpgaImagesResponse'
    { fpgaImages = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the FPGA images.
--
-- /Note:/ Consider using 'fpgaImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirsFpgaImages :: Lens.Lens' DescribeFpgaImagesResponse (Lude.Maybe [FpgaImage])
dfirsFpgaImages = Lens.lens (fpgaImages :: DescribeFpgaImagesResponse -> Lude.Maybe [FpgaImage]) (\s a -> s {fpgaImages = a} :: DescribeFpgaImagesResponse)
{-# DEPRECATED dfirsFpgaImages "Use generic-lens or generic-optics with 'fpgaImages' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirsNextToken :: Lens.Lens' DescribeFpgaImagesResponse (Lude.Maybe Lude.Text)
dfirsNextToken = Lens.lens (nextToken :: DescribeFpgaImagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFpgaImagesResponse)
{-# DEPRECATED dfirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirsResponseStatus :: Lens.Lens' DescribeFpgaImagesResponse Lude.Int
dfirsResponseStatus = Lens.lens (responseStatus :: DescribeFpgaImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFpgaImagesResponse)
{-# DEPRECATED dfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
