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
-- Module      : Network.AWS.IoT.ListActiveViolations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active violations for a given Device Defender security profile.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListActiveViolations
  ( -- * Creating a Request
    listActiveViolations,
    ListActiveViolations,

    -- * Request Lenses
    lavNextToken,
    lavSecurityProfileName,
    lavThingName,
    lavMaxResults,

    -- * Destructuring the Response
    listActiveViolationsResponse,
    ListActiveViolationsResponse,

    -- * Response Lenses
    lavrsActiveViolations,
    lavrsNextToken,
    lavrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listActiveViolations' smart constructor.
data ListActiveViolations = ListActiveViolations'
  { _lavNextToken ::
      !(Maybe Text),
    _lavSecurityProfileName :: !(Maybe Text),
    _lavThingName :: !(Maybe Text),
    _lavMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListActiveViolations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavNextToken' - The token for the next set of results.
--
-- * 'lavSecurityProfileName' - The name of the Device Defender security profile for which violations are listed.
--
-- * 'lavThingName' - The name of the thing whose active violations are listed.
--
-- * 'lavMaxResults' - The maximum number of results to return at one time.
listActiveViolations ::
  ListActiveViolations
listActiveViolations =
  ListActiveViolations'
    { _lavNextToken = Nothing,
      _lavSecurityProfileName = Nothing,
      _lavThingName = Nothing,
      _lavMaxResults = Nothing
    }

-- | The token for the next set of results.
lavNextToken :: Lens' ListActiveViolations (Maybe Text)
lavNextToken = lens _lavNextToken (\s a -> s {_lavNextToken = a})

-- | The name of the Device Defender security profile for which violations are listed.
lavSecurityProfileName :: Lens' ListActiveViolations (Maybe Text)
lavSecurityProfileName = lens _lavSecurityProfileName (\s a -> s {_lavSecurityProfileName = a})

-- | The name of the thing whose active violations are listed.
lavThingName :: Lens' ListActiveViolations (Maybe Text)
lavThingName = lens _lavThingName (\s a -> s {_lavThingName = a})

-- | The maximum number of results to return at one time.
lavMaxResults :: Lens' ListActiveViolations (Maybe Natural)
lavMaxResults = lens _lavMaxResults (\s a -> s {_lavMaxResults = a}) . mapping _Nat

instance AWSPager ListActiveViolations where
  page rq rs
    | stop (rs ^. lavrsNextToken) = Nothing
    | stop (rs ^. lavrsActiveViolations) = Nothing
    | otherwise = Just $ rq & lavNextToken .~ rs ^. lavrsNextToken

instance AWSRequest ListActiveViolations where
  type Rs ListActiveViolations = ListActiveViolationsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListActiveViolationsResponse'
            <$> (x .?> "activeViolations" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListActiveViolations

instance NFData ListActiveViolations

instance ToHeaders ListActiveViolations where
  toHeaders = const mempty

instance ToPath ListActiveViolations where
  toPath = const "/active-violations"

instance ToQuery ListActiveViolations where
  toQuery ListActiveViolations' {..} =
    mconcat
      [ "nextToken" =: _lavNextToken,
        "securityProfileName" =: _lavSecurityProfileName,
        "thingName" =: _lavThingName,
        "maxResults" =: _lavMaxResults
      ]

-- | /See:/ 'listActiveViolationsResponse' smart constructor.
data ListActiveViolationsResponse = ListActiveViolationsResponse'
  { _lavrsActiveViolations ::
      !(Maybe [ActiveViolation]),
    _lavrsNextToken :: !(Maybe Text),
    _lavrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListActiveViolationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavrsActiveViolations' - The list of active violations.
--
-- * 'lavrsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lavrsResponseStatus' - -- | The response status code.
listActiveViolationsResponse ::
  -- | 'lavrsResponseStatus'
  Int ->
  ListActiveViolationsResponse
listActiveViolationsResponse pResponseStatus_ =
  ListActiveViolationsResponse'
    { _lavrsActiveViolations = Nothing,
      _lavrsNextToken = Nothing,
      _lavrsResponseStatus = pResponseStatus_
    }

-- | The list of active violations.
lavrsActiveViolations :: Lens' ListActiveViolationsResponse [ActiveViolation]
lavrsActiveViolations = lens _lavrsActiveViolations (\s a -> s {_lavrsActiveViolations = a}) . _Default . _Coerce

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lavrsNextToken :: Lens' ListActiveViolationsResponse (Maybe Text)
lavrsNextToken = lens _lavrsNextToken (\s a -> s {_lavrsNextToken = a})

-- | -- | The response status code.
lavrsResponseStatus :: Lens' ListActiveViolationsResponse Int
lavrsResponseStatus = lens _lavrsResponseStatus (\s a -> s {_lavrsResponseStatus = a})

instance NFData ListActiveViolationsResponse
