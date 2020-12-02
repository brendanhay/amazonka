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
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePackStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization conformance pack deployment status for an organization.
--
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConformancePackStatuses
  ( -- * Creating a Request
    describeOrganizationConformancePackStatuses,
    DescribeOrganizationConformancePackStatuses,

    -- * Request Lenses
    docpsNextToken,
    docpsLimit,
    docpsOrganizationConformancePackNames,

    -- * Destructuring the Response
    describeOrganizationConformancePackStatusesResponse,
    DescribeOrganizationConformancePackStatusesResponse,

    -- * Response Lenses
    docpsrsOrganizationConformancePackStatuses,
    docpsrsNextToken,
    docpsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganizationConformancePackStatuses' smart constructor.
data DescribeOrganizationConformancePackStatuses = DescribeOrganizationConformancePackStatuses'
  { _docpsNextToken ::
      !( Maybe
           Text
       ),
    _docpsLimit ::
      !( Maybe
           Nat
       ),
    _docpsOrganizationConformancePackNames ::
      !( Maybe
           [Text]
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeOrganizationConformancePackStatuses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docpsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docpsLimit' - The maximum number of OrganizationConformancePackStatuses returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- * 'docpsOrganizationConformancePackNames' - The names of organization conformance packs for which you want status details. If you do not specify any names, AWS Config returns details for all your organization conformance packs.
describeOrganizationConformancePackStatuses ::
  DescribeOrganizationConformancePackStatuses
describeOrganizationConformancePackStatuses =
  DescribeOrganizationConformancePackStatuses'
    { _docpsNextToken =
        Nothing,
      _docpsLimit = Nothing,
      _docpsOrganizationConformancePackNames = Nothing
    }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
docpsNextToken :: Lens' DescribeOrganizationConformancePackStatuses (Maybe Text)
docpsNextToken = lens _docpsNextToken (\s a -> s {_docpsNextToken = a})

-- | The maximum number of OrganizationConformancePackStatuses returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
docpsLimit :: Lens' DescribeOrganizationConformancePackStatuses (Maybe Natural)
docpsLimit = lens _docpsLimit (\s a -> s {_docpsLimit = a}) . mapping _Nat

-- | The names of organization conformance packs for which you want status details. If you do not specify any names, AWS Config returns details for all your organization conformance packs.
docpsOrganizationConformancePackNames :: Lens' DescribeOrganizationConformancePackStatuses [Text]
docpsOrganizationConformancePackNames = lens _docpsOrganizationConformancePackNames (\s a -> s {_docpsOrganizationConformancePackNames = a}) . _Default . _Coerce

instance AWSRequest DescribeOrganizationConformancePackStatuses where
  type
    Rs DescribeOrganizationConformancePackStatuses =
      DescribeOrganizationConformancePackStatusesResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePackStatusesResponse'
            <$> (x .?> "OrganizationConformancePackStatuses" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOrganizationConformancePackStatuses

instance NFData DescribeOrganizationConformancePackStatuses

instance ToHeaders DescribeOrganizationConformancePackStatuses where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeOrganizationConformancePackStatuses" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeOrganizationConformancePackStatuses where
  toJSON DescribeOrganizationConformancePackStatuses' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _docpsNextToken,
            ("Limit" .=) <$> _docpsLimit,
            ("OrganizationConformancePackNames" .=)
              <$> _docpsOrganizationConformancePackNames
          ]
      )

instance ToPath DescribeOrganizationConformancePackStatuses where
  toPath = const "/"

instance ToQuery DescribeOrganizationConformancePackStatuses where
  toQuery = const mempty

-- | /See:/ 'describeOrganizationConformancePackStatusesResponse' smart constructor.
data DescribeOrganizationConformancePackStatusesResponse = DescribeOrganizationConformancePackStatusesResponse'
  { _docpsrsOrganizationConformancePackStatuses ::
      !( Maybe
           [OrganizationConformancePackStatus]
       ),
    _docpsrsNextToken ::
      !( Maybe
           Text
       ),
    _docpsrsResponseStatus ::
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

-- | Creates a value of 'DescribeOrganizationConformancePackStatusesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docpsrsOrganizationConformancePackStatuses' - A list of @OrganizationConformancePackStatus@ objects.
--
-- * 'docpsrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docpsrsResponseStatus' - -- | The response status code.
describeOrganizationConformancePackStatusesResponse ::
  -- | 'docpsrsResponseStatus'
  Int ->
  DescribeOrganizationConformancePackStatusesResponse
describeOrganizationConformancePackStatusesResponse
  pResponseStatus_ =
    DescribeOrganizationConformancePackStatusesResponse'
      { _docpsrsOrganizationConformancePackStatuses =
          Nothing,
        _docpsrsNextToken = Nothing,
        _docpsrsResponseStatus = pResponseStatus_
      }

-- | A list of @OrganizationConformancePackStatus@ objects.
docpsrsOrganizationConformancePackStatuses :: Lens' DescribeOrganizationConformancePackStatusesResponse [OrganizationConformancePackStatus]
docpsrsOrganizationConformancePackStatuses = lens _docpsrsOrganizationConformancePackStatuses (\s a -> s {_docpsrsOrganizationConformancePackStatuses = a}) . _Default . _Coerce

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
docpsrsNextToken :: Lens' DescribeOrganizationConformancePackStatusesResponse (Maybe Text)
docpsrsNextToken = lens _docpsrsNextToken (\s a -> s {_docpsrsNextToken = a})

-- | -- | The response status code.
docpsrsResponseStatus :: Lens' DescribeOrganizationConformancePackStatusesResponse Int
docpsrsResponseStatus = lens _docpsrsResponseStatus (\s a -> s {_docpsrsResponseStatus = a})

instance NFData DescribeOrganizationConformancePackStatusesResponse
