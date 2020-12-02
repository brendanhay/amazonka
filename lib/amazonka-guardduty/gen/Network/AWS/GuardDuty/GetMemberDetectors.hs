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
-- Module      : Network.AWS.GuardDuty.GetMemberDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes which data sources are enabled for the member account's detector.
module Network.AWS.GuardDuty.GetMemberDetectors
  ( -- * Creating a Request
    getMemberDetectors,
    GetMemberDetectors,

    -- * Request Lenses
    gmdDetectorId,
    gmdAccountIds,

    -- * Destructuring the Response
    getMemberDetectorsResponse,
    GetMemberDetectorsResponse,

    -- * Response Lenses
    gmdrsResponseStatus,
    gmdrsMemberDataSourceConfigurations,
    gmdrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMemberDetectors' smart constructor.
data GetMemberDetectors = GetMemberDetectors'
  { _gmdDetectorId ::
      !Text,
    _gmdAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMemberDetectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdDetectorId' - The detector ID for the master account.
--
-- * 'gmdAccountIds' - The account ID of the member account.
getMemberDetectors ::
  -- | 'gmdDetectorId'
  Text ->
  -- | 'gmdAccountIds'
  NonEmpty Text ->
  GetMemberDetectors
getMemberDetectors pDetectorId_ pAccountIds_ =
  GetMemberDetectors'
    { _gmdDetectorId = pDetectorId_,
      _gmdAccountIds = _List1 # pAccountIds_
    }

-- | The detector ID for the master account.
gmdDetectorId :: Lens' GetMemberDetectors Text
gmdDetectorId = lens _gmdDetectorId (\s a -> s {_gmdDetectorId = a})

-- | The account ID of the member account.
gmdAccountIds :: Lens' GetMemberDetectors (NonEmpty Text)
gmdAccountIds = lens _gmdAccountIds (\s a -> s {_gmdAccountIds = a}) . _List1

instance AWSRequest GetMemberDetectors where
  type Rs GetMemberDetectors = GetMemberDetectorsResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetMemberDetectorsResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "members")
            <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable GetMemberDetectors

instance NFData GetMemberDetectors

instance ToHeaders GetMemberDetectors where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetMemberDetectors where
  toJSON GetMemberDetectors' {..} =
    object (catMaybes [Just ("accountIds" .= _gmdAccountIds)])

instance ToPath GetMemberDetectors where
  toPath GetMemberDetectors' {..} =
    mconcat
      ["/detector/", toBS _gmdDetectorId, "/member/detector/get"]

instance ToQuery GetMemberDetectors where
  toQuery = const mempty

-- | /See:/ 'getMemberDetectorsResponse' smart constructor.
data GetMemberDetectorsResponse = GetMemberDetectorsResponse'
  { _gmdrsResponseStatus ::
      !Int,
    _gmdrsMemberDataSourceConfigurations ::
      !( List1
           MemberDataSourceConfiguration
       ),
    _gmdrsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMemberDetectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdrsResponseStatus' - -- | The response status code.
--
-- * 'gmdrsMemberDataSourceConfigurations' - An object that describes which data sources are enabled for a member account.
--
-- * 'gmdrsUnprocessedAccounts' - A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
getMemberDetectorsResponse ::
  -- | 'gmdrsResponseStatus'
  Int ->
  -- | 'gmdrsMemberDataSourceConfigurations'
  NonEmpty MemberDataSourceConfiguration ->
  GetMemberDetectorsResponse
getMemberDetectorsResponse
  pResponseStatus_
  pMemberDataSourceConfigurations_ =
    GetMemberDetectorsResponse'
      { _gmdrsResponseStatus =
          pResponseStatus_,
        _gmdrsMemberDataSourceConfigurations =
          _List1 # pMemberDataSourceConfigurations_,
        _gmdrsUnprocessedAccounts = mempty
      }

-- | -- | The response status code.
gmdrsResponseStatus :: Lens' GetMemberDetectorsResponse Int
gmdrsResponseStatus = lens _gmdrsResponseStatus (\s a -> s {_gmdrsResponseStatus = a})

-- | An object that describes which data sources are enabled for a member account.
gmdrsMemberDataSourceConfigurations :: Lens' GetMemberDetectorsResponse (NonEmpty MemberDataSourceConfiguration)
gmdrsMemberDataSourceConfigurations = lens _gmdrsMemberDataSourceConfigurations (\s a -> s {_gmdrsMemberDataSourceConfigurations = a}) . _List1

-- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
gmdrsUnprocessedAccounts :: Lens' GetMemberDetectorsResponse [UnprocessedAccount]
gmdrsUnprocessedAccounts = lens _gmdrsUnprocessedAccounts (\s a -> s {_gmdrsUnprocessedAccounts = a}) . _Coerce

instance NFData GetMemberDetectorsResponse
