{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFpgaImages
  ( -- * Creating a Request
    describeFpgaImages,
    DescribeFpgaImages,

    -- * Request Lenses
    dfifOwners,
    dfifFilters,
    dfifNextToken,
    dfifDryRun,
    dfifMaxResults,
    dfifFpgaImageIds,

    -- * Destructuring the Response
    describeFpgaImagesResponse,
    DescribeFpgaImagesResponse,

    -- * Response Lenses
    dfirsFpgaImages,
    dfirsNextToken,
    dfirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFpgaImages' smart constructor.
data DescribeFpgaImages = DescribeFpgaImages'
  { _dfifOwners ::
      !(Maybe [Text]),
    _dfifFilters :: !(Maybe [Filter]),
    _dfifNextToken :: !(Maybe Text),
    _dfifDryRun :: !(Maybe Bool),
    _dfifMaxResults :: !(Maybe Nat),
    _dfifFpgaImageIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFpgaImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfifOwners' - Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
--
-- * 'dfifFilters' - The filters.     * @create-time@ - The creation time of the AFI.     * @fpga-image-id@ - The FPGA image identifier (AFI ID).     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).     * @name@ - The name of the AFI.     * @owner-id@ - The AWS account ID of the AFI owner.     * @product-code@ - The product code.     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @update-time@ - The time of the most recent update.
--
-- * 'dfifNextToken' - The token to retrieve the next page of results.
--
-- * 'dfifDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfifMaxResults' - The maximum number of results to return in a single call.
--
-- * 'dfifFpgaImageIds' - The AFI IDs.
describeFpgaImages ::
  DescribeFpgaImages
describeFpgaImages =
  DescribeFpgaImages'
    { _dfifOwners = Nothing,
      _dfifFilters = Nothing,
      _dfifNextToken = Nothing,
      _dfifDryRun = Nothing,
      _dfifMaxResults = Nothing,
      _dfifFpgaImageIds = Nothing
    }

-- | Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is the sender of the request), or an AWS owner alias (valid values are @amazon@ | @aws-marketplace@ ).
dfifOwners :: Lens' DescribeFpgaImages [Text]
dfifOwners = lens _dfifOwners (\s a -> s {_dfifOwners = a}) . _Default . _Coerce

-- | The filters.     * @create-time@ - The creation time of the AFI.     * @fpga-image-id@ - The FPGA image identifier (AFI ID).     * @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).     * @name@ - The name of the AFI.     * @owner-id@ - The AWS account ID of the AFI owner.     * @product-code@ - The product code.     * @shell-version@ - The version of the AWS Shell that was used to create the bitstream.     * @state@ - The state of the AFI (@pending@ | @failed@ | @available@ | @unavailable@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @update-time@ - The time of the most recent update.
dfifFilters :: Lens' DescribeFpgaImages [Filter]
dfifFilters = lens _dfifFilters (\s a -> s {_dfifFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dfifNextToken :: Lens' DescribeFpgaImages (Maybe Text)
dfifNextToken = lens _dfifNextToken (\s a -> s {_dfifNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfifDryRun :: Lens' DescribeFpgaImages (Maybe Bool)
dfifDryRun = lens _dfifDryRun (\s a -> s {_dfifDryRun = a})

-- | The maximum number of results to return in a single call.
dfifMaxResults :: Lens' DescribeFpgaImages (Maybe Natural)
dfifMaxResults = lens _dfifMaxResults (\s a -> s {_dfifMaxResults = a}) . mapping _Nat

-- | The AFI IDs.
dfifFpgaImageIds :: Lens' DescribeFpgaImages [Text]
dfifFpgaImageIds = lens _dfifFpgaImageIds (\s a -> s {_dfifFpgaImageIds = a}) . _Default . _Coerce

instance AWSPager DescribeFpgaImages where
  page rq rs
    | stop (rs ^. dfirsNextToken) = Nothing
    | stop (rs ^. dfirsFpgaImages) = Nothing
    | otherwise = Just $ rq & dfifNextToken .~ rs ^. dfirsNextToken

instance AWSRequest DescribeFpgaImages where
  type Rs DescribeFpgaImages = DescribeFpgaImagesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeFpgaImagesResponse'
            <$> (x .@? "fpgaImageSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeFpgaImages

instance NFData DescribeFpgaImages

instance ToHeaders DescribeFpgaImages where
  toHeaders = const mempty

instance ToPath DescribeFpgaImages where
  toPath = const "/"

instance ToQuery DescribeFpgaImages where
  toQuery DescribeFpgaImages' {..} =
    mconcat
      [ "Action" =: ("DescribeFpgaImages" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Owner" <$> _dfifOwners),
        toQuery (toQueryList "Filter" <$> _dfifFilters),
        "NextToken" =: _dfifNextToken,
        "DryRun" =: _dfifDryRun,
        "MaxResults" =: _dfifMaxResults,
        toQuery (toQueryList "FpgaImageId" <$> _dfifFpgaImageIds)
      ]

-- | /See:/ 'describeFpgaImagesResponse' smart constructor.
data DescribeFpgaImagesResponse = DescribeFpgaImagesResponse'
  { _dfirsFpgaImages ::
      !(Maybe [FpgaImage]),
    _dfirsNextToken :: !(Maybe Text),
    _dfirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFpgaImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfirsFpgaImages' - Information about the FPGA images.
--
-- * 'dfirsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dfirsResponseStatus' - -- | The response status code.
describeFpgaImagesResponse ::
  -- | 'dfirsResponseStatus'
  Int ->
  DescribeFpgaImagesResponse
describeFpgaImagesResponse pResponseStatus_ =
  DescribeFpgaImagesResponse'
    { _dfirsFpgaImages = Nothing,
      _dfirsNextToken = Nothing,
      _dfirsResponseStatus = pResponseStatus_
    }

-- | Information about the FPGA images.
dfirsFpgaImages :: Lens' DescribeFpgaImagesResponse [FpgaImage]
dfirsFpgaImages = lens _dfirsFpgaImages (\s a -> s {_dfirsFpgaImages = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dfirsNextToken :: Lens' DescribeFpgaImagesResponse (Maybe Text)
dfirsNextToken = lens _dfirsNextToken (\s a -> s {_dfirsNextToken = a})

-- | -- | The response status code.
dfirsResponseStatus :: Lens' DescribeFpgaImagesResponse Int
dfirsResponseStatus = lens _dfirsResponseStatus (\s a -> s {_dfirsResponseStatus = a})

instance NFData DescribeFpgaImagesResponse
