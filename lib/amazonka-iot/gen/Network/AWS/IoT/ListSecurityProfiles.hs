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
-- Module      : Network.AWS.IoT.ListSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles you have created. You can use filters to list only those security profiles associated with a thing group or only those associated with your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfiles
  ( -- * Creating a Request
    listSecurityProfiles,
    ListSecurityProfiles,

    -- * Request Lenses
    lspNextToken,
    lspDimensionName,
    lspMaxResults,

    -- * Destructuring the Response
    listSecurityProfilesResponse,
    ListSecurityProfilesResponse,

    -- * Response Lenses
    lsprsNextToken,
    lsprsSecurityProfileIdentifiers,
    lsprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { _lspNextToken ::
      !(Maybe Text),
    _lspDimensionName :: !(Maybe Text),
    _lspMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspNextToken' - The token for the next set of results.
--
-- * 'lspDimensionName' - A filter to limit results to the security profiles that use the defined dimension.
--
-- * 'lspMaxResults' - The maximum number of results to return at one time.
listSecurityProfiles ::
  ListSecurityProfiles
listSecurityProfiles =
  ListSecurityProfiles'
    { _lspNextToken = Nothing,
      _lspDimensionName = Nothing,
      _lspMaxResults = Nothing
    }

-- | The token for the next set of results.
lspNextToken :: Lens' ListSecurityProfiles (Maybe Text)
lspNextToken = lens _lspNextToken (\s a -> s {_lspNextToken = a})

-- | A filter to limit results to the security profiles that use the defined dimension.
lspDimensionName :: Lens' ListSecurityProfiles (Maybe Text)
lspDimensionName = lens _lspDimensionName (\s a -> s {_lspDimensionName = a})

-- | The maximum number of results to return at one time.
lspMaxResults :: Lens' ListSecurityProfiles (Maybe Natural)
lspMaxResults = lens _lspMaxResults (\s a -> s {_lspMaxResults = a}) . mapping _Nat

instance AWSPager ListSecurityProfiles where
  page rq rs
    | stop (rs ^. lsprsNextToken) = Nothing
    | stop (rs ^. lsprsSecurityProfileIdentifiers) = Nothing
    | otherwise = Just $ rq & lspNextToken .~ rs ^. lsprsNextToken

instance AWSRequest ListSecurityProfiles where
  type Rs ListSecurityProfiles = ListSecurityProfilesResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "securityProfileIdentifiers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListSecurityProfiles

instance NFData ListSecurityProfiles

instance ToHeaders ListSecurityProfiles where
  toHeaders = const mempty

instance ToPath ListSecurityProfiles where
  toPath = const "/security-profiles"

instance ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    mconcat
      [ "nextToken" =: _lspNextToken,
        "dimensionName" =: _lspDimensionName,
        "maxResults" =: _lspMaxResults
      ]

-- | /See:/ 'listSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { _lsprsNextToken ::
      !(Maybe Text),
    _lsprsSecurityProfileIdentifiers ::
      !( Maybe
           [SecurityProfileIdentifier]
       ),
    _lsprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsprsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lsprsSecurityProfileIdentifiers' - A list of security profile identifiers (names and ARNs).
--
-- * 'lsprsResponseStatus' - -- | The response status code.
listSecurityProfilesResponse ::
  -- | 'lsprsResponseStatus'
  Int ->
  ListSecurityProfilesResponse
listSecurityProfilesResponse pResponseStatus_ =
  ListSecurityProfilesResponse'
    { _lsprsNextToken = Nothing,
      _lsprsSecurityProfileIdentifiers = Nothing,
      _lsprsResponseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lsprsNextToken :: Lens' ListSecurityProfilesResponse (Maybe Text)
lsprsNextToken = lens _lsprsNextToken (\s a -> s {_lsprsNextToken = a})

-- | A list of security profile identifiers (names and ARNs).
lsprsSecurityProfileIdentifiers :: Lens' ListSecurityProfilesResponse [SecurityProfileIdentifier]
lsprsSecurityProfileIdentifiers = lens _lsprsSecurityProfileIdentifiers (\s a -> s {_lsprsSecurityProfileIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
lsprsResponseStatus :: Lens' ListSecurityProfilesResponse Int
lsprsResponseStatus = lens _lsprsResponseStatus (\s a -> s {_lsprsResponseStatus = a})

instance NFData ListSecurityProfilesResponse
