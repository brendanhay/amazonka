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
-- Module      : Network.AWS.ECR.DescribeImageScanFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the scan findings for the specified image.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImageScanFindings
  ( -- * Creating a Request
    describeImageScanFindings,
    DescribeImageScanFindings,

    -- * Request Lenses
    disfRegistryId,
    disfNextToken,
    disfMaxResults,
    disfRepositoryName,
    disfImageId,

    -- * Destructuring the Response
    describeImageScanFindingsResponse,
    DescribeImageScanFindingsResponse,

    -- * Response Lenses
    disfrsRegistryId,
    disfrsImageScanFindings,
    disfrsImageScanStatus,
    disfrsNextToken,
    disfrsImageId,
    disfrsRepositoryName,
    disfrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImageScanFindings' smart constructor.
data DescribeImageScanFindings = DescribeImageScanFindings'
  { _disfRegistryId ::
      !(Maybe Text),
    _disfNextToken :: !(Maybe Text),
    _disfMaxResults :: !(Maybe Nat),
    _disfRepositoryName :: !Text,
    _disfImageId :: !ImageIdentifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImageScanFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disfRegistryId' - The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
--
-- * 'disfNextToken' - The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
--
-- * 'disfMaxResults' - The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
--
-- * 'disfRepositoryName' - The repository for the image for which to describe the scan findings.
--
-- * 'disfImageId' - Undocumented member.
describeImageScanFindings ::
  -- | 'disfRepositoryName'
  Text ->
  -- | 'disfImageId'
  ImageIdentifier ->
  DescribeImageScanFindings
describeImageScanFindings pRepositoryName_ pImageId_ =
  DescribeImageScanFindings'
    { _disfRegistryId = Nothing,
      _disfNextToken = Nothing,
      _disfMaxResults = Nothing,
      _disfRepositoryName = pRepositoryName_,
      _disfImageId = pImageId_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
disfRegistryId :: Lens' DescribeImageScanFindings (Maybe Text)
disfRegistryId = lens _disfRegistryId (\s a -> s {_disfRegistryId = a})

-- | The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
disfNextToken :: Lens' DescribeImageScanFindings (Maybe Text)
disfNextToken = lens _disfNextToken (\s a -> s {_disfNextToken = a})

-- | The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
disfMaxResults :: Lens' DescribeImageScanFindings (Maybe Natural)
disfMaxResults = lens _disfMaxResults (\s a -> s {_disfMaxResults = a}) . mapping _Nat

-- | The repository for the image for which to describe the scan findings.
disfRepositoryName :: Lens' DescribeImageScanFindings Text
disfRepositoryName = lens _disfRepositoryName (\s a -> s {_disfRepositoryName = a})

-- | Undocumented member.
disfImageId :: Lens' DescribeImageScanFindings ImageIdentifier
disfImageId = lens _disfImageId (\s a -> s {_disfImageId = a})

instance AWSPager DescribeImageScanFindings where
  page rq rs
    | stop (rs ^. disfrsNextToken) = Nothing
    | stop (rs ^? disfrsImageScanFindings . _Just . isfFindings) =
      Nothing
    | otherwise = Just $ rq & disfNextToken .~ rs ^. disfrsNextToken

instance AWSRequest DescribeImageScanFindings where
  type
    Rs DescribeImageScanFindings =
      DescribeImageScanFindingsResponse
  request = postJSON ecr
  response =
    receiveJSON
      ( \s h x ->
          DescribeImageScanFindingsResponse'
            <$> (x .?> "registryId")
            <*> (x .?> "imageScanFindings")
            <*> (x .?> "imageScanStatus")
            <*> (x .?> "nextToken")
            <*> (x .?> "imageId")
            <*> (x .?> "repositoryName")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeImageScanFindings

instance NFData DescribeImageScanFindings

instance ToHeaders DescribeImageScanFindings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImageScanFindings" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeImageScanFindings where
  toJSON DescribeImageScanFindings' {..} =
    object
      ( catMaybes
          [ ("registryId" .=) <$> _disfRegistryId,
            ("nextToken" .=) <$> _disfNextToken,
            ("maxResults" .=) <$> _disfMaxResults,
            Just ("repositoryName" .= _disfRepositoryName),
            Just ("imageId" .= _disfImageId)
          ]
      )

instance ToPath DescribeImageScanFindings where
  toPath = const "/"

instance ToQuery DescribeImageScanFindings where
  toQuery = const mempty

-- | /See:/ 'describeImageScanFindingsResponse' smart constructor.
data DescribeImageScanFindingsResponse = DescribeImageScanFindingsResponse'
  { _disfrsRegistryId ::
      !(Maybe Text),
    _disfrsImageScanFindings ::
      !( Maybe
           ImageScanFindings
       ),
    _disfrsImageScanStatus ::
      !( Maybe
           ImageScanStatus
       ),
    _disfrsNextToken ::
      !(Maybe Text),
    _disfrsImageId ::
      !( Maybe
           ImageIdentifier
       ),
    _disfrsRepositoryName ::
      !(Maybe Text),
    _disfrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImageScanFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disfrsRegistryId' - The registry ID associated with the request.
--
-- * 'disfrsImageScanFindings' - The information contained in the image scan findings.
--
-- * 'disfrsImageScanStatus' - The current state of the scan.
--
-- * 'disfrsNextToken' - The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
--
-- * 'disfrsImageId' - Undocumented member.
--
-- * 'disfrsRepositoryName' - The repository name associated with the request.
--
-- * 'disfrsResponseStatus' - -- | The response status code.
describeImageScanFindingsResponse ::
  -- | 'disfrsResponseStatus'
  Int ->
  DescribeImageScanFindingsResponse
describeImageScanFindingsResponse pResponseStatus_ =
  DescribeImageScanFindingsResponse'
    { _disfrsRegistryId = Nothing,
      _disfrsImageScanFindings = Nothing,
      _disfrsImageScanStatus = Nothing,
      _disfrsNextToken = Nothing,
      _disfrsImageId = Nothing,
      _disfrsRepositoryName = Nothing,
      _disfrsResponseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
disfrsRegistryId :: Lens' DescribeImageScanFindingsResponse (Maybe Text)
disfrsRegistryId = lens _disfrsRegistryId (\s a -> s {_disfrsRegistryId = a})

-- | The information contained in the image scan findings.
disfrsImageScanFindings :: Lens' DescribeImageScanFindingsResponse (Maybe ImageScanFindings)
disfrsImageScanFindings = lens _disfrsImageScanFindings (\s a -> s {_disfrsImageScanFindings = a})

-- | The current state of the scan.
disfrsImageScanStatus :: Lens' DescribeImageScanFindingsResponse (Maybe ImageScanStatus)
disfrsImageScanStatus = lens _disfrsImageScanStatus (\s a -> s {_disfrsImageScanStatus = a})

-- | The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
disfrsNextToken :: Lens' DescribeImageScanFindingsResponse (Maybe Text)
disfrsNextToken = lens _disfrsNextToken (\s a -> s {_disfrsNextToken = a})

-- | Undocumented member.
disfrsImageId :: Lens' DescribeImageScanFindingsResponse (Maybe ImageIdentifier)
disfrsImageId = lens _disfrsImageId (\s a -> s {_disfrsImageId = a})

-- | The repository name associated with the request.
disfrsRepositoryName :: Lens' DescribeImageScanFindingsResponse (Maybe Text)
disfrsRepositoryName = lens _disfrsRepositoryName (\s a -> s {_disfrsRepositoryName = a})

-- | -- | The response status code.
disfrsResponseStatus :: Lens' DescribeImageScanFindingsResponse Int
disfrsResponseStatus = lens _disfrsResponseStatus (\s a -> s {_disfrsResponseStatus = a})

instance NFData DescribeImageScanFindingsResponse
