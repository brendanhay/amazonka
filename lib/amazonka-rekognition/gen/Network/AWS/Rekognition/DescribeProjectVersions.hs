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
-- Module      : Network.AWS.Rekognition.DescribeProjectVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and describes the models in an Amazon Rekognition Custom Labels project. You can specify up to 10 model versions in @ProjectVersionArns@ . If you don't specify a value, descriptions for all models are returned.
--
--
-- This operation requires permissions to perform the @rekognition:DescribeProjectVersions@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjectVersions
  ( -- * Creating a Request
    describeProjectVersions,
    DescribeProjectVersions,

    -- * Request Lenses
    dpvNextToken,
    dpvVersionNames,
    dpvMaxResults,
    dpvProjectARN,

    -- * Destructuring the Response
    describeProjectVersionsResponse,
    DescribeProjectVersionsResponse,

    -- * Response Lenses
    dpvrsNextToken,
    dpvrsProjectVersionDescriptions,
    dpvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeProjectVersions' smart constructor.
data DescribeProjectVersions = DescribeProjectVersions'
  { _dpvNextToken ::
      !(Maybe Text),
    _dpvVersionNames :: !(Maybe (List1 Text)),
    _dpvMaxResults :: !(Maybe Nat),
    _dpvProjectARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProjectVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvNextToken' - If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- * 'dpvVersionNames' - A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
--
-- * 'dpvMaxResults' - The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
--
-- * 'dpvProjectARN' - The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
describeProjectVersions ::
  -- | 'dpvProjectARN'
  Text ->
  DescribeProjectVersions
describeProjectVersions pProjectARN_ =
  DescribeProjectVersions'
    { _dpvNextToken = Nothing,
      _dpvVersionNames = Nothing,
      _dpvMaxResults = Nothing,
      _dpvProjectARN = pProjectARN_
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
dpvNextToken :: Lens' DescribeProjectVersions (Maybe Text)
dpvNextToken = lens _dpvNextToken (\s a -> s {_dpvNextToken = a})

-- | A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
dpvVersionNames :: Lens' DescribeProjectVersions (Maybe (NonEmpty Text))
dpvVersionNames = lens _dpvVersionNames (\s a -> s {_dpvVersionNames = a}) . mapping _List1

-- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
dpvMaxResults :: Lens' DescribeProjectVersions (Maybe Natural)
dpvMaxResults = lens _dpvMaxResults (\s a -> s {_dpvMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
dpvProjectARN :: Lens' DescribeProjectVersions Text
dpvProjectARN = lens _dpvProjectARN (\s a -> s {_dpvProjectARN = a})

instance AWSPager DescribeProjectVersions where
  page rq rs
    | stop (rs ^. dpvrsNextToken) = Nothing
    | stop (rs ^. dpvrsProjectVersionDescriptions) = Nothing
    | otherwise = Just $ rq & dpvNextToken .~ rs ^. dpvrsNextToken

instance AWSRequest DescribeProjectVersions where
  type Rs DescribeProjectVersions = DescribeProjectVersionsResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          DescribeProjectVersionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "ProjectVersionDescriptions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeProjectVersions

instance NFData DescribeProjectVersions

instance ToHeaders DescribeProjectVersions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.DescribeProjectVersions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeProjectVersions where
  toJSON DescribeProjectVersions' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dpvNextToken,
            ("VersionNames" .=) <$> _dpvVersionNames,
            ("MaxResults" .=) <$> _dpvMaxResults,
            Just ("ProjectArn" .= _dpvProjectARN)
          ]
      )

instance ToPath DescribeProjectVersions where
  toPath = const "/"

instance ToQuery DescribeProjectVersions where
  toQuery = const mempty

-- | /See:/ 'describeProjectVersionsResponse' smart constructor.
data DescribeProjectVersionsResponse = DescribeProjectVersionsResponse'
  { _dpvrsNextToken ::
      !(Maybe Text),
    _dpvrsProjectVersionDescriptions ::
      !( Maybe
           [ProjectVersionDescription]
       ),
    _dpvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProjectVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvrsNextToken' - If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- * 'dpvrsProjectVersionDescriptions' - A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
--
-- * 'dpvrsResponseStatus' - -- | The response status code.
describeProjectVersionsResponse ::
  -- | 'dpvrsResponseStatus'
  Int ->
  DescribeProjectVersionsResponse
describeProjectVersionsResponse pResponseStatus_ =
  DescribeProjectVersionsResponse'
    { _dpvrsNextToken = Nothing,
      _dpvrsProjectVersionDescriptions = Nothing,
      _dpvrsResponseStatus = pResponseStatus_
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
dpvrsNextToken :: Lens' DescribeProjectVersionsResponse (Maybe Text)
dpvrsNextToken = lens _dpvrsNextToken (\s a -> s {_dpvrsNextToken = a})

-- | A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
dpvrsProjectVersionDescriptions :: Lens' DescribeProjectVersionsResponse [ProjectVersionDescription]
dpvrsProjectVersionDescriptions = lens _dpvrsProjectVersionDescriptions (\s a -> s {_dpvrsProjectVersionDescriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dpvrsResponseStatus :: Lens' DescribeProjectVersionsResponse Int
dpvrsResponseStatus = lens _dpvrsResponseStatus (\s a -> s {_dpvrsResponseStatus = a})

instance NFData DescribeProjectVersionsResponse
