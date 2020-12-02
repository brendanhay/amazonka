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
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization conformance packs.
--
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConformancePacks
  ( -- * Creating a Request
    describeOrganizationConformancePacks,
    DescribeOrganizationConformancePacks,

    -- * Request Lenses
    docpNextToken,
    docpLimit,
    docpOrganizationConformancePackNames,

    -- * Destructuring the Response
    describeOrganizationConformancePacksResponse,
    DescribeOrganizationConformancePacksResponse,

    -- * Response Lenses
    docprsOrganizationConformancePacks,
    docprsNextToken,
    docprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganizationConformancePacks' smart constructor.
data DescribeOrganizationConformancePacks = DescribeOrganizationConformancePacks'
  { _docpNextToken ::
      !(Maybe Text),
    _docpLimit ::
      !(Maybe Nat),
    _docpOrganizationConformancePackNames ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganizationConformancePacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docpNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docpLimit' - The maximum number of organization config packs returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- * 'docpOrganizationConformancePackNames' - The name that you assign to an organization conformance pack.
describeOrganizationConformancePacks ::
  DescribeOrganizationConformancePacks
describeOrganizationConformancePacks =
  DescribeOrganizationConformancePacks'
    { _docpNextToken = Nothing,
      _docpLimit = Nothing,
      _docpOrganizationConformancePackNames = Nothing
    }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
docpNextToken :: Lens' DescribeOrganizationConformancePacks (Maybe Text)
docpNextToken = lens _docpNextToken (\s a -> s {_docpNextToken = a})

-- | The maximum number of organization config packs returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
docpLimit :: Lens' DescribeOrganizationConformancePacks (Maybe Natural)
docpLimit = lens _docpLimit (\s a -> s {_docpLimit = a}) . mapping _Nat

-- | The name that you assign to an organization conformance pack.
docpOrganizationConformancePackNames :: Lens' DescribeOrganizationConformancePacks [Text]
docpOrganizationConformancePackNames = lens _docpOrganizationConformancePackNames (\s a -> s {_docpOrganizationConformancePackNames = a}) . _Default . _Coerce

instance AWSRequest DescribeOrganizationConformancePacks where
  type
    Rs DescribeOrganizationConformancePacks =
      DescribeOrganizationConformancePacksResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePacksResponse'
            <$> (x .?> "OrganizationConformancePacks" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOrganizationConformancePacks

instance NFData DescribeOrganizationConformancePacks

instance ToHeaders DescribeOrganizationConformancePacks where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeOrganizationConformancePacks" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeOrganizationConformancePacks where
  toJSON DescribeOrganizationConformancePacks' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _docpNextToken,
            ("Limit" .=) <$> _docpLimit,
            ("OrganizationConformancePackNames" .=)
              <$> _docpOrganizationConformancePackNames
          ]
      )

instance ToPath DescribeOrganizationConformancePacks where
  toPath = const "/"

instance ToQuery DescribeOrganizationConformancePacks where
  toQuery = const mempty

-- | /See:/ 'describeOrganizationConformancePacksResponse' smart constructor.
data DescribeOrganizationConformancePacksResponse = DescribeOrganizationConformancePacksResponse'
  { _docprsOrganizationConformancePacks ::
      !( Maybe
           [OrganizationConformancePack]
       ),
    _docprsNextToken ::
      !( Maybe
           Text
       ),
    _docprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeOrganizationConformancePacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docprsOrganizationConformancePacks' - Returns a list of OrganizationConformancePacks objects.
--
-- * 'docprsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docprsResponseStatus' - -- | The response status code.
describeOrganizationConformancePacksResponse ::
  -- | 'docprsResponseStatus'
  Int ->
  DescribeOrganizationConformancePacksResponse
describeOrganizationConformancePacksResponse pResponseStatus_ =
  DescribeOrganizationConformancePacksResponse'
    { _docprsOrganizationConformancePacks =
        Nothing,
      _docprsNextToken = Nothing,
      _docprsResponseStatus = pResponseStatus_
    }

-- | Returns a list of OrganizationConformancePacks objects.
docprsOrganizationConformancePacks :: Lens' DescribeOrganizationConformancePacksResponse [OrganizationConformancePack]
docprsOrganizationConformancePacks = lens _docprsOrganizationConformancePacks (\s a -> s {_docprsOrganizationConformancePacks = a}) . _Default . _Coerce

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
docprsNextToken :: Lens' DescribeOrganizationConformancePacksResponse (Maybe Text)
docprsNextToken = lens _docprsNextToken (\s a -> s {_docprsNextToken = a})

-- | -- | The response status code.
docprsResponseStatus :: Lens' DescribeOrganizationConformancePacksResponse Int
docprsResponseStatus = lens _docprsResponseStatus (\s a -> s {_docprsResponseStatus = a})

instance NFData DescribeOrganizationConformancePacksResponse
