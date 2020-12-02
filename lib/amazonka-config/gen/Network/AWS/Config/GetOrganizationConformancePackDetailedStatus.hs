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
-- Module      : Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization for a given organization conformance pack.
--
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
  ( -- * Creating a Request
    getOrganizationConformancePackDetailedStatus,
    GetOrganizationConformancePackDetailedStatus,

    -- * Request Lenses
    gocpdsFilters,
    gocpdsNextToken,
    gocpdsLimit,
    gocpdsOrganizationConformancePackName,

    -- * Destructuring the Response
    getOrganizationConformancePackDetailedStatusResponse,
    GetOrganizationConformancePackDetailedStatusResponse,

    -- * Response Lenses
    gocpdsrsOrganizationConformancePackDetailedStatuses,
    gocpdsrsNextToken,
    gocpdsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOrganizationConformancePackDetailedStatus' smart constructor.
data GetOrganizationConformancePackDetailedStatus = GetOrganizationConformancePackDetailedStatus'
  { _gocpdsFilters ::
      !( Maybe
           OrganizationResourceDetailedStatusFilters
       ),
    _gocpdsNextToken ::
      !( Maybe
           Text
       ),
    _gocpdsLimit ::
      !( Maybe
           Nat
       ),
    _gocpdsOrganizationConformancePackName ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetOrganizationConformancePackDetailedStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gocpdsFilters' - An @OrganizationResourceDetailedStatusFilters@ object.
--
-- * 'gocpdsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gocpdsLimit' - The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
--
-- * 'gocpdsOrganizationConformancePackName' - The name of organization conformance pack for which you want status details for member accounts.
getOrganizationConformancePackDetailedStatus ::
  -- | 'gocpdsOrganizationConformancePackName'
  Text ->
  GetOrganizationConformancePackDetailedStatus
getOrganizationConformancePackDetailedStatus
  pOrganizationConformancePackName_ =
    GetOrganizationConformancePackDetailedStatus'
      { _gocpdsFilters =
          Nothing,
        _gocpdsNextToken = Nothing,
        _gocpdsLimit = Nothing,
        _gocpdsOrganizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | An @OrganizationResourceDetailedStatusFilters@ object.
gocpdsFilters :: Lens' GetOrganizationConformancePackDetailedStatus (Maybe OrganizationResourceDetailedStatusFilters)
gocpdsFilters = lens _gocpdsFilters (\s a -> s {_gocpdsFilters = a})

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gocpdsNextToken :: Lens' GetOrganizationConformancePackDetailedStatus (Maybe Text)
gocpdsNextToken = lens _gocpdsNextToken (\s a -> s {_gocpdsNextToken = a})

-- | The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
gocpdsLimit :: Lens' GetOrganizationConformancePackDetailedStatus (Maybe Natural)
gocpdsLimit = lens _gocpdsLimit (\s a -> s {_gocpdsLimit = a}) . mapping _Nat

-- | The name of organization conformance pack for which you want status details for member accounts.
gocpdsOrganizationConformancePackName :: Lens' GetOrganizationConformancePackDetailedStatus Text
gocpdsOrganizationConformancePackName = lens _gocpdsOrganizationConformancePackName (\s a -> s {_gocpdsOrganizationConformancePackName = a})

instance AWSRequest GetOrganizationConformancePackDetailedStatus where
  type
    Rs GetOrganizationConformancePackDetailedStatus =
      GetOrganizationConformancePackDetailedStatusResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          GetOrganizationConformancePackDetailedStatusResponse'
            <$> (x .?> "OrganizationConformancePackDetailedStatuses" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetOrganizationConformancePackDetailedStatus

instance NFData GetOrganizationConformancePackDetailedStatus

instance ToHeaders GetOrganizationConformancePackDetailedStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.GetOrganizationConformancePackDetailedStatus" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetOrganizationConformancePackDetailedStatus where
  toJSON GetOrganizationConformancePackDetailedStatus' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _gocpdsFilters,
            ("NextToken" .=) <$> _gocpdsNextToken,
            ("Limit" .=) <$> _gocpdsLimit,
            Just
              ( "OrganizationConformancePackName"
                  .= _gocpdsOrganizationConformancePackName
              )
          ]
      )

instance ToPath GetOrganizationConformancePackDetailedStatus where
  toPath = const "/"

instance ToQuery GetOrganizationConformancePackDetailedStatus where
  toQuery = const mempty

-- | /See:/ 'getOrganizationConformancePackDetailedStatusResponse' smart constructor.
data GetOrganizationConformancePackDetailedStatusResponse = GetOrganizationConformancePackDetailedStatusResponse'
  { _gocpdsrsOrganizationConformancePackDetailedStatuses ::
      !( Maybe
           [OrganizationConformancePackDetailedStatus]
       ),
    _gocpdsrsNextToken ::
      !( Maybe
           Text
       ),
    _gocpdsrsResponseStatus ::
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

-- | Creates a value of 'GetOrganizationConformancePackDetailedStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gocpdsrsOrganizationConformancePackDetailedStatuses' - A list of @OrganizationConformancePackDetailedStatus@ objects.
--
-- * 'gocpdsrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gocpdsrsResponseStatus' - -- | The response status code.
getOrganizationConformancePackDetailedStatusResponse ::
  -- | 'gocpdsrsResponseStatus'
  Int ->
  GetOrganizationConformancePackDetailedStatusResponse
getOrganizationConformancePackDetailedStatusResponse
  pResponseStatus_ =
    GetOrganizationConformancePackDetailedStatusResponse'
      { _gocpdsrsOrganizationConformancePackDetailedStatuses =
          Nothing,
        _gocpdsrsNextToken = Nothing,
        _gocpdsrsResponseStatus =
          pResponseStatus_
      }

-- | A list of @OrganizationConformancePackDetailedStatus@ objects.
gocpdsrsOrganizationConformancePackDetailedStatuses :: Lens' GetOrganizationConformancePackDetailedStatusResponse [OrganizationConformancePackDetailedStatus]
gocpdsrsOrganizationConformancePackDetailedStatuses = lens _gocpdsrsOrganizationConformancePackDetailedStatuses (\s a -> s {_gocpdsrsOrganizationConformancePackDetailedStatuses = a}) . _Default . _Coerce

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gocpdsrsNextToken :: Lens' GetOrganizationConformancePackDetailedStatusResponse (Maybe Text)
gocpdsrsNextToken = lens _gocpdsrsNextToken (\s a -> s {_gocpdsrsNextToken = a})

-- | -- | The response status code.
gocpdsrsResponseStatus :: Lens' GetOrganizationConformancePackDetailedStatusResponse Int
gocpdsrsResponseStatus = lens _gocpdsrsResponseStatus (\s a -> s {_gocpdsrsResponseStatus = a})

instance
  NFData
    GetOrganizationConformancePackDetailedStatusResponse
