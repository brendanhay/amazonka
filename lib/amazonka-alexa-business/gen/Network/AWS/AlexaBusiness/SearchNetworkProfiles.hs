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
-- Module      : Network.AWS.AlexaBusiness.SearchNetworkProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches network profiles and lists the ones that meet a set of filter and sort criteria.
module Network.AWS.AlexaBusiness.SearchNetworkProfiles
  ( -- * Creating a Request
    searchNetworkProfiles,
    SearchNetworkProfiles,

    -- * Request Lenses
    snpFilters,
    snpSortCriteria,
    snpNextToken,
    snpMaxResults,

    -- * Destructuring the Response
    searchNetworkProfilesResponse,
    SearchNetworkProfilesResponse,

    -- * Response Lenses
    snprsNetworkProfiles,
    snprsNextToken,
    snprsTotalCount,
    snprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchNetworkProfiles' smart constructor.
data SearchNetworkProfiles = SearchNetworkProfiles'
  { _snpFilters ::
      !(Maybe [Filter]),
    _snpSortCriteria :: !(Maybe [Sort]),
    _snpNextToken :: !(Maybe Text),
    _snpMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchNetworkProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snpFilters' - The filters to use to list a specified set of network profiles. Valid filters are NetworkProfileName, Ssid, and SecurityType.
--
-- * 'snpSortCriteria' - The sort order to use to list the specified set of network profiles. Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
--
-- * 'snpNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
--
-- * 'snpMaxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
searchNetworkProfiles ::
  SearchNetworkProfiles
searchNetworkProfiles =
  SearchNetworkProfiles'
    { _snpFilters = Nothing,
      _snpSortCriteria = Nothing,
      _snpNextToken = Nothing,
      _snpMaxResults = Nothing
    }

-- | The filters to use to list a specified set of network profiles. Valid filters are NetworkProfileName, Ssid, and SecurityType.
snpFilters :: Lens' SearchNetworkProfiles [Filter]
snpFilters = lens _snpFilters (\s a -> s {_snpFilters = a}) . _Default . _Coerce

-- | The sort order to use to list the specified set of network profiles. Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
snpSortCriteria :: Lens' SearchNetworkProfiles [Sort]
snpSortCriteria = lens _snpSortCriteria (\s a -> s {_snpSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
snpNextToken :: Lens' SearchNetworkProfiles (Maybe Text)
snpNextToken = lens _snpNextToken (\s a -> s {_snpNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
snpMaxResults :: Lens' SearchNetworkProfiles (Maybe Natural)
snpMaxResults = lens _snpMaxResults (\s a -> s {_snpMaxResults = a}) . mapping _Nat

instance AWSRequest SearchNetworkProfiles where
  type Rs SearchNetworkProfiles = SearchNetworkProfilesResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          SearchNetworkProfilesResponse'
            <$> (x .?> "NetworkProfiles" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (x .?> "TotalCount")
            <*> (pure (fromEnum s))
      )

instance Hashable SearchNetworkProfiles

instance NFData SearchNetworkProfiles

instance ToHeaders SearchNetworkProfiles where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.SearchNetworkProfiles" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SearchNetworkProfiles where
  toJSON SearchNetworkProfiles' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _snpFilters,
            ("SortCriteria" .=) <$> _snpSortCriteria,
            ("NextToken" .=) <$> _snpNextToken,
            ("MaxResults" .=) <$> _snpMaxResults
          ]
      )

instance ToPath SearchNetworkProfiles where
  toPath = const "/"

instance ToQuery SearchNetworkProfiles where
  toQuery = const mempty

-- | /See:/ 'searchNetworkProfilesResponse' smart constructor.
data SearchNetworkProfilesResponse = SearchNetworkProfilesResponse'
  { _snprsNetworkProfiles ::
      !(Maybe [NetworkProfileData]),
    _snprsNextToken ::
      !(Maybe Text),
    _snprsTotalCount ::
      !(Maybe Int),
    _snprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchNetworkProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snprsNetworkProfiles' - The network profiles that meet the specified set of filter criteria, in sort order. It is a list of NetworkProfileData objects.
--
-- * 'snprsNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
--
-- * 'snprsTotalCount' - The total number of network profiles returned.
--
-- * 'snprsResponseStatus' - -- | The response status code.
searchNetworkProfilesResponse ::
  -- | 'snprsResponseStatus'
  Int ->
  SearchNetworkProfilesResponse
searchNetworkProfilesResponse pResponseStatus_ =
  SearchNetworkProfilesResponse'
    { _snprsNetworkProfiles = Nothing,
      _snprsNextToken = Nothing,
      _snprsTotalCount = Nothing,
      _snprsResponseStatus = pResponseStatus_
    }

-- | The network profiles that meet the specified set of filter criteria, in sort order. It is a list of NetworkProfileData objects.
snprsNetworkProfiles :: Lens' SearchNetworkProfilesResponse [NetworkProfileData]
snprsNetworkProfiles = lens _snprsNetworkProfiles (\s a -> s {_snprsNetworkProfiles = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
snprsNextToken :: Lens' SearchNetworkProfilesResponse (Maybe Text)
snprsNextToken = lens _snprsNextToken (\s a -> s {_snprsNextToken = a})

-- | The total number of network profiles returned.
snprsTotalCount :: Lens' SearchNetworkProfilesResponse (Maybe Int)
snprsTotalCount = lens _snprsTotalCount (\s a -> s {_snprsTotalCount = a})

-- | -- | The response status code.
snprsResponseStatus :: Lens' SearchNetworkProfilesResponse Int
snprsResponseStatus = lens _snprsResponseStatus (\s a -> s {_snprsResponseStatus = a})

instance NFData SearchNetworkProfilesResponse
