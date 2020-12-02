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
-- Module      : Network.AWS.EC2.GetManagedPrefixListEntries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the entries for a specified managed prefix list.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListEntries
  ( -- * Creating a Request
    getManagedPrefixListEntries,
    GetManagedPrefixListEntries,

    -- * Request Lenses
    gmpleNextToken,
    gmpleTargetVersion,
    gmpleDryRun,
    gmpleMaxResults,
    gmplePrefixListId,

    -- * Destructuring the Response
    getManagedPrefixListEntriesResponse,
    GetManagedPrefixListEntriesResponse,

    -- * Response Lenses
    gmplersEntries,
    gmplersNextToken,
    gmplersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getManagedPrefixListEntries' smart constructor.
data GetManagedPrefixListEntries = GetManagedPrefixListEntries'
  { _gmpleNextToken ::
      !(Maybe Text),
    _gmpleTargetVersion ::
      !(Maybe Integer),
    _gmpleDryRun :: !(Maybe Bool),
    _gmpleMaxResults :: !(Maybe Nat),
    _gmplePrefixListId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetManagedPrefixListEntries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmpleNextToken' - The token for the next page of results.
--
-- * 'gmpleTargetVersion' - The version of the prefix list for which to return the entries. The default is the current version.
--
-- * 'gmpleDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gmpleMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'gmplePrefixListId' - The ID of the prefix list.
getManagedPrefixListEntries ::
  -- | 'gmplePrefixListId'
  Text ->
  GetManagedPrefixListEntries
getManagedPrefixListEntries pPrefixListId_ =
  GetManagedPrefixListEntries'
    { _gmpleNextToken = Nothing,
      _gmpleTargetVersion = Nothing,
      _gmpleDryRun = Nothing,
      _gmpleMaxResults = Nothing,
      _gmplePrefixListId = pPrefixListId_
    }

-- | The token for the next page of results.
gmpleNextToken :: Lens' GetManagedPrefixListEntries (Maybe Text)
gmpleNextToken = lens _gmpleNextToken (\s a -> s {_gmpleNextToken = a})

-- | The version of the prefix list for which to return the entries. The default is the current version.
gmpleTargetVersion :: Lens' GetManagedPrefixListEntries (Maybe Integer)
gmpleTargetVersion = lens _gmpleTargetVersion (\s a -> s {_gmpleTargetVersion = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gmpleDryRun :: Lens' GetManagedPrefixListEntries (Maybe Bool)
gmpleDryRun = lens _gmpleDryRun (\s a -> s {_gmpleDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gmpleMaxResults :: Lens' GetManagedPrefixListEntries (Maybe Natural)
gmpleMaxResults = lens _gmpleMaxResults (\s a -> s {_gmpleMaxResults = a}) . mapping _Nat

-- | The ID of the prefix list.
gmplePrefixListId :: Lens' GetManagedPrefixListEntries Text
gmplePrefixListId = lens _gmplePrefixListId (\s a -> s {_gmplePrefixListId = a})

instance AWSPager GetManagedPrefixListEntries where
  page rq rs
    | stop (rs ^. gmplersNextToken) = Nothing
    | stop (rs ^. gmplersEntries) = Nothing
    | otherwise = Just $ rq & gmpleNextToken .~ rs ^. gmplersNextToken

instance AWSRequest GetManagedPrefixListEntries where
  type
    Rs GetManagedPrefixListEntries =
      GetManagedPrefixListEntriesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetManagedPrefixListEntriesResponse'
            <$> (x .@? "entrySet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetManagedPrefixListEntries

instance NFData GetManagedPrefixListEntries

instance ToHeaders GetManagedPrefixListEntries where
  toHeaders = const mempty

instance ToPath GetManagedPrefixListEntries where
  toPath = const "/"

instance ToQuery GetManagedPrefixListEntries where
  toQuery GetManagedPrefixListEntries' {..} =
    mconcat
      [ "Action" =: ("GetManagedPrefixListEntries" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NextToken" =: _gmpleNextToken,
        "TargetVersion" =: _gmpleTargetVersion,
        "DryRun" =: _gmpleDryRun,
        "MaxResults" =: _gmpleMaxResults,
        "PrefixListId" =: _gmplePrefixListId
      ]

-- | /See:/ 'getManagedPrefixListEntriesResponse' smart constructor.
data GetManagedPrefixListEntriesResponse = GetManagedPrefixListEntriesResponse'
  { _gmplersEntries ::
      !( Maybe
           [PrefixListEntry]
       ),
    _gmplersNextToken ::
      !(Maybe Text),
    _gmplersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetManagedPrefixListEntriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmplersEntries' - Information about the prefix list entries.
--
-- * 'gmplersNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gmplersResponseStatus' - -- | The response status code.
getManagedPrefixListEntriesResponse ::
  -- | 'gmplersResponseStatus'
  Int ->
  GetManagedPrefixListEntriesResponse
getManagedPrefixListEntriesResponse pResponseStatus_ =
  GetManagedPrefixListEntriesResponse'
    { _gmplersEntries = Nothing,
      _gmplersNextToken = Nothing,
      _gmplersResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list entries.
gmplersEntries :: Lens' GetManagedPrefixListEntriesResponse [PrefixListEntry]
gmplersEntries = lens _gmplersEntries (\s a -> s {_gmplersEntries = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gmplersNextToken :: Lens' GetManagedPrefixListEntriesResponse (Maybe Text)
gmplersNextToken = lens _gmplersNextToken (\s a -> s {_gmplersNextToken = a})

-- | -- | The response status code.
gmplersResponseStatus :: Lens' GetManagedPrefixListEntriesResponse Int
gmplersResponseStatus = lens _gmplersResponseStatus (\s a -> s {_gmplersResponseStatus = a})

instance NFData GetManagedPrefixListEntriesResponse
