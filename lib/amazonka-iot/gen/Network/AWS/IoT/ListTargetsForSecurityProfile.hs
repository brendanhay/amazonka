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
-- Module      : Network.AWS.IoT.ListTargetsForSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets (thing groups) associated with a given Device Defender security profile.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForSecurityProfile
  ( -- * Creating a Request
    listTargetsForSecurityProfile,
    ListTargetsForSecurityProfile,

    -- * Request Lenses
    ltfspNextToken,
    ltfspMaxResults,
    ltfspSecurityProfileName,

    -- * Destructuring the Response
    listTargetsForSecurityProfileResponse,
    ListTargetsForSecurityProfileResponse,

    -- * Response Lenses
    ltfsprsSecurityProfileTargets,
    ltfsprsNextToken,
    ltfsprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTargetsForSecurityProfile' smart constructor.
data ListTargetsForSecurityProfile = ListTargetsForSecurityProfile'
  { _ltfspNextToken ::
      !(Maybe Text),
    _ltfspMaxResults ::
      !(Maybe Nat),
    _ltfspSecurityProfileName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTargetsForSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfspNextToken' - The token for the next set of results.
--
-- * 'ltfspMaxResults' - The maximum number of results to return at one time.
--
-- * 'ltfspSecurityProfileName' - The security profile.
listTargetsForSecurityProfile ::
  -- | 'ltfspSecurityProfileName'
  Text ->
  ListTargetsForSecurityProfile
listTargetsForSecurityProfile pSecurityProfileName_ =
  ListTargetsForSecurityProfile'
    { _ltfspNextToken = Nothing,
      _ltfspMaxResults = Nothing,
      _ltfspSecurityProfileName = pSecurityProfileName_
    }

-- | The token for the next set of results.
ltfspNextToken :: Lens' ListTargetsForSecurityProfile (Maybe Text)
ltfspNextToken = lens _ltfspNextToken (\s a -> s {_ltfspNextToken = a})

-- | The maximum number of results to return at one time.
ltfspMaxResults :: Lens' ListTargetsForSecurityProfile (Maybe Natural)
ltfspMaxResults = lens _ltfspMaxResults (\s a -> s {_ltfspMaxResults = a}) . mapping _Nat

-- | The security profile.
ltfspSecurityProfileName :: Lens' ListTargetsForSecurityProfile Text
ltfspSecurityProfileName = lens _ltfspSecurityProfileName (\s a -> s {_ltfspSecurityProfileName = a})

instance AWSPager ListTargetsForSecurityProfile where
  page rq rs
    | stop (rs ^. ltfsprsNextToken) = Nothing
    | stop (rs ^. ltfsprsSecurityProfileTargets) = Nothing
    | otherwise = Just $ rq & ltfspNextToken .~ rs ^. ltfsprsNextToken

instance AWSRequest ListTargetsForSecurityProfile where
  type
    Rs ListTargetsForSecurityProfile =
      ListTargetsForSecurityProfileResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListTargetsForSecurityProfileResponse'
            <$> (x .?> "securityProfileTargets" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTargetsForSecurityProfile

instance NFData ListTargetsForSecurityProfile

instance ToHeaders ListTargetsForSecurityProfile where
  toHeaders = const mempty

instance ToPath ListTargetsForSecurityProfile where
  toPath ListTargetsForSecurityProfile' {..} =
    mconcat
      ["/security-profiles/", toBS _ltfspSecurityProfileName, "/targets"]

instance ToQuery ListTargetsForSecurityProfile where
  toQuery ListTargetsForSecurityProfile' {..} =
    mconcat
      ["nextToken" =: _ltfspNextToken, "maxResults" =: _ltfspMaxResults]

-- | /See:/ 'listTargetsForSecurityProfileResponse' smart constructor.
data ListTargetsForSecurityProfileResponse = ListTargetsForSecurityProfileResponse'
  { _ltfsprsSecurityProfileTargets ::
      !( Maybe
           [SecurityProfileTarget]
       ),
    _ltfsprsNextToken ::
      !(Maybe Text),
    _ltfsprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTargetsForSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfsprsSecurityProfileTargets' - The thing groups to which the security profile is attached.
--
-- * 'ltfsprsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'ltfsprsResponseStatus' - -- | The response status code.
listTargetsForSecurityProfileResponse ::
  -- | 'ltfsprsResponseStatus'
  Int ->
  ListTargetsForSecurityProfileResponse
listTargetsForSecurityProfileResponse pResponseStatus_ =
  ListTargetsForSecurityProfileResponse'
    { _ltfsprsSecurityProfileTargets =
        Nothing,
      _ltfsprsNextToken = Nothing,
      _ltfsprsResponseStatus = pResponseStatus_
    }

-- | The thing groups to which the security profile is attached.
ltfsprsSecurityProfileTargets :: Lens' ListTargetsForSecurityProfileResponse [SecurityProfileTarget]
ltfsprsSecurityProfileTargets = lens _ltfsprsSecurityProfileTargets (\s a -> s {_ltfsprsSecurityProfileTargets = a}) . _Default . _Coerce

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
ltfsprsNextToken :: Lens' ListTargetsForSecurityProfileResponse (Maybe Text)
ltfsprsNextToken = lens _ltfsprsNextToken (\s a -> s {_ltfsprsNextToken = a})

-- | -- | The response status code.
ltfsprsResponseStatus :: Lens' ListTargetsForSecurityProfileResponse Int
ltfsprsResponseStatus = lens _ltfsprsResponseStatus (\s a -> s {_ltfsprsResponseStatus = a})

instance NFData ListTargetsForSecurityProfileResponse
