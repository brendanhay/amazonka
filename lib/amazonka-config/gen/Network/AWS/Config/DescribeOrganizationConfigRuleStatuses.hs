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
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization config rule deployment status for an organization.
--
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
  ( -- * Creating a Request
    describeOrganizationConfigRuleStatuses,
    DescribeOrganizationConfigRuleStatuses,

    -- * Request Lenses
    docrsOrganizationConfigRuleNames,
    docrsNextToken,
    docrsLimit,

    -- * Destructuring the Response
    describeOrganizationConfigRuleStatusesResponse,
    DescribeOrganizationConfigRuleStatusesResponse,

    -- * Response Lenses
    docrsrsNextToken,
    docrsrsOrganizationConfigRuleStatuses,
    docrsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganizationConfigRuleStatuses' smart constructor.
data DescribeOrganizationConfigRuleStatuses = DescribeOrganizationConfigRuleStatuses'
  { _docrsOrganizationConfigRuleNames ::
      !( Maybe
           [Text]
       ),
    _docrsNextToken ::
      !(Maybe Text),
    _docrsLimit ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganizationConfigRuleStatuses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docrsOrganizationConfigRuleNames' - The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
--
-- * 'docrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docrsLimit' - The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
describeOrganizationConfigRuleStatuses ::
  DescribeOrganizationConfigRuleStatuses
describeOrganizationConfigRuleStatuses =
  DescribeOrganizationConfigRuleStatuses'
    { _docrsOrganizationConfigRuleNames =
        Nothing,
      _docrsNextToken = Nothing,
      _docrsLimit = Nothing
    }

-- | The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
docrsOrganizationConfigRuleNames :: Lens' DescribeOrganizationConfigRuleStatuses [Text]
docrsOrganizationConfigRuleNames = lens _docrsOrganizationConfigRuleNames (\s a -> s {_docrsOrganizationConfigRuleNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
docrsNextToken :: Lens' DescribeOrganizationConfigRuleStatuses (Maybe Text)
docrsNextToken = lens _docrsNextToken (\s a -> s {_docrsNextToken = a})

-- | The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
docrsLimit :: Lens' DescribeOrganizationConfigRuleStatuses (Maybe Natural)
docrsLimit = lens _docrsLimit (\s a -> s {_docrsLimit = a}) . mapping _Nat

instance AWSRequest DescribeOrganizationConfigRuleStatuses where
  type
    Rs DescribeOrganizationConfigRuleStatuses =
      DescribeOrganizationConfigRuleStatusesResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRuleStatusesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "OrganizationConfigRuleStatuses" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOrganizationConfigRuleStatuses

instance NFData DescribeOrganizationConfigRuleStatuses

instance ToHeaders DescribeOrganizationConfigRuleStatuses where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeOrganizationConfigRuleStatuses" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeOrganizationConfigRuleStatuses where
  toJSON DescribeOrganizationConfigRuleStatuses' {..} =
    object
      ( catMaybes
          [ ("OrganizationConfigRuleNames" .=)
              <$> _docrsOrganizationConfigRuleNames,
            ("NextToken" .=) <$> _docrsNextToken,
            ("Limit" .=) <$> _docrsLimit
          ]
      )

instance ToPath DescribeOrganizationConfigRuleStatuses where
  toPath = const "/"

instance ToQuery DescribeOrganizationConfigRuleStatuses where
  toQuery = const mempty

-- | /See:/ 'describeOrganizationConfigRuleStatusesResponse' smart constructor.
data DescribeOrganizationConfigRuleStatusesResponse = DescribeOrganizationConfigRuleStatusesResponse'
  { _docrsrsNextToken ::
      !( Maybe
           Text
       ),
    _docrsrsOrganizationConfigRuleStatuses ::
      !( Maybe
           [OrganizationConfigRuleStatus]
       ),
    _docrsrsResponseStatus ::
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

-- | Creates a value of 'DescribeOrganizationConfigRuleStatusesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docrsrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docrsrsOrganizationConfigRuleStatuses' - A list of @OrganizationConfigRuleStatus@ objects.
--
-- * 'docrsrsResponseStatus' - -- | The response status code.
describeOrganizationConfigRuleStatusesResponse ::
  -- | 'docrsrsResponseStatus'
  Int ->
  DescribeOrganizationConfigRuleStatusesResponse
describeOrganizationConfigRuleStatusesResponse pResponseStatus_ =
  DescribeOrganizationConfigRuleStatusesResponse'
    { _docrsrsNextToken =
        Nothing,
      _docrsrsOrganizationConfigRuleStatuses =
        Nothing,
      _docrsrsResponseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
docrsrsNextToken :: Lens' DescribeOrganizationConfigRuleStatusesResponse (Maybe Text)
docrsrsNextToken = lens _docrsrsNextToken (\s a -> s {_docrsrsNextToken = a})

-- | A list of @OrganizationConfigRuleStatus@ objects.
docrsrsOrganizationConfigRuleStatuses :: Lens' DescribeOrganizationConfigRuleStatusesResponse [OrganizationConfigRuleStatus]
docrsrsOrganizationConfigRuleStatuses = lens _docrsrsOrganizationConfigRuleStatuses (\s a -> s {_docrsrsOrganizationConfigRuleStatuses = a}) . _Default . _Coerce

-- | -- | The response status code.
docrsrsResponseStatus :: Lens' DescribeOrganizationConfigRuleStatusesResponse Int
docrsrsResponseStatus = lens _docrsrsResponseStatus (\s a -> s {_docrsrsResponseStatus = a})

instance NFData DescribeOrganizationConfigRuleStatusesResponse
