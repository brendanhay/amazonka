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
-- Module      : Network.AWS.IoT.ListSecurityProfilesForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles attached to a target (thing group).
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfilesForTarget
  ( -- * Creating a Request
    listSecurityProfilesForTarget,
    ListSecurityProfilesForTarget,

    -- * Request Lenses
    lspftNextToken,
    lspftRecursive,
    lspftMaxResults,
    lspftSecurityProfileTargetARN,

    -- * Destructuring the Response
    listSecurityProfilesForTargetResponse,
    ListSecurityProfilesForTargetResponse,

    -- * Response Lenses
    lspftrsNextToken,
    lspftrsSecurityProfileTargetMappings,
    lspftrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSecurityProfilesForTarget' smart constructor.
data ListSecurityProfilesForTarget = ListSecurityProfilesForTarget'
  { _lspftNextToken ::
      !(Maybe Text),
    _lspftRecursive ::
      !(Maybe Bool),
    _lspftMaxResults ::
      !(Maybe Nat),
    _lspftSecurityProfileTargetARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityProfilesForTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspftNextToken' - The token for the next set of results.
--
-- * 'lspftRecursive' - If true, return child groups too.
--
-- * 'lspftMaxResults' - The maximum number of results to return at one time.
--
-- * 'lspftSecurityProfileTargetARN' - The ARN of the target (thing group) whose attached security profiles you want to get.
listSecurityProfilesForTarget ::
  -- | 'lspftSecurityProfileTargetARN'
  Text ->
  ListSecurityProfilesForTarget
listSecurityProfilesForTarget pSecurityProfileTargetARN_ =
  ListSecurityProfilesForTarget'
    { _lspftNextToken = Nothing,
      _lspftRecursive = Nothing,
      _lspftMaxResults = Nothing,
      _lspftSecurityProfileTargetARN = pSecurityProfileTargetARN_
    }

-- | The token for the next set of results.
lspftNextToken :: Lens' ListSecurityProfilesForTarget (Maybe Text)
lspftNextToken = lens _lspftNextToken (\s a -> s {_lspftNextToken = a})

-- | If true, return child groups too.
lspftRecursive :: Lens' ListSecurityProfilesForTarget (Maybe Bool)
lspftRecursive = lens _lspftRecursive (\s a -> s {_lspftRecursive = a})

-- | The maximum number of results to return at one time.
lspftMaxResults :: Lens' ListSecurityProfilesForTarget (Maybe Natural)
lspftMaxResults = lens _lspftMaxResults (\s a -> s {_lspftMaxResults = a}) . mapping _Nat

-- | The ARN of the target (thing group) whose attached security profiles you want to get.
lspftSecurityProfileTargetARN :: Lens' ListSecurityProfilesForTarget Text
lspftSecurityProfileTargetARN = lens _lspftSecurityProfileTargetARN (\s a -> s {_lspftSecurityProfileTargetARN = a})

instance AWSPager ListSecurityProfilesForTarget where
  page rq rs
    | stop (rs ^. lspftrsNextToken) = Nothing
    | stop (rs ^. lspftrsSecurityProfileTargetMappings) = Nothing
    | otherwise = Just $ rq & lspftNextToken .~ rs ^. lspftrsNextToken

instance AWSRequest ListSecurityProfilesForTarget where
  type
    Rs ListSecurityProfilesForTarget =
      ListSecurityProfilesForTargetResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListSecurityProfilesForTargetResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "securityProfileTargetMappings" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListSecurityProfilesForTarget

instance NFData ListSecurityProfilesForTarget

instance ToHeaders ListSecurityProfilesForTarget where
  toHeaders = const mempty

instance ToPath ListSecurityProfilesForTarget where
  toPath = const "/security-profiles-for-target"

instance ToQuery ListSecurityProfilesForTarget where
  toQuery ListSecurityProfilesForTarget' {..} =
    mconcat
      [ "nextToken" =: _lspftNextToken,
        "recursive" =: _lspftRecursive,
        "maxResults" =: _lspftMaxResults,
        "securityProfileTargetArn" =: _lspftSecurityProfileTargetARN
      ]

-- | /See:/ 'listSecurityProfilesForTargetResponse' smart constructor.
data ListSecurityProfilesForTargetResponse = ListSecurityProfilesForTargetResponse'
  { _lspftrsNextToken ::
      !(Maybe Text),
    _lspftrsSecurityProfileTargetMappings ::
      !( Maybe
           [SecurityProfileTargetMapping]
       ),
    _lspftrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityProfilesForTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspftrsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lspftrsSecurityProfileTargetMappings' - A list of security profiles and their associated targets.
--
-- * 'lspftrsResponseStatus' - -- | The response status code.
listSecurityProfilesForTargetResponse ::
  -- | 'lspftrsResponseStatus'
  Int ->
  ListSecurityProfilesForTargetResponse
listSecurityProfilesForTargetResponse pResponseStatus_ =
  ListSecurityProfilesForTargetResponse'
    { _lspftrsNextToken =
        Nothing,
      _lspftrsSecurityProfileTargetMappings = Nothing,
      _lspftrsResponseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lspftrsNextToken :: Lens' ListSecurityProfilesForTargetResponse (Maybe Text)
lspftrsNextToken = lens _lspftrsNextToken (\s a -> s {_lspftrsNextToken = a})

-- | A list of security profiles and their associated targets.
lspftrsSecurityProfileTargetMappings :: Lens' ListSecurityProfilesForTargetResponse [SecurityProfileTargetMapping]
lspftrsSecurityProfileTargetMappings = lens _lspftrsSecurityProfileTargetMappings (\s a -> s {_lspftrsSecurityProfileTargetMappings = a}) . _Default . _Coerce

-- | -- | The response status code.
lspftrsResponseStatus :: Lens' ListSecurityProfilesForTargetResponse Int
lspftrsResponseStatus = lens _lspftrsResponseStatus (\s a -> s {_lspftrsResponseStatus = a})

instance NFData ListSecurityProfilesForTargetResponse
