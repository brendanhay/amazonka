{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.DescribeChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns an array of Challenge-Handshake Authentication
-- Protocol (CHAP) credentials information for a specified iSCSI target,
-- one for each target-initiator pair.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeChapCredentials.html>
module Network.AWS.StorageGateway.DescribeChapCredentials
    (
    -- * Request
      DescribeChapCredentials
    -- ** Request constructor
    , describeChapCredentials
    -- ** Request lenses
    , dccTargetARN

    -- * Response
    , DescribeChapCredentialsResponse
    -- ** Response constructor
    , describeChapCredentialsResponse
    -- ** Response lenses
    , dccrChapCredentials
    , dccrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the iSCSI
-- volume target.
--
-- /See:/ 'describeChapCredentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccTargetARN'
newtype DescribeChapCredentials = DescribeChapCredentials'
    { _dccTargetARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeChapCredentials' smart constructor.
describeChapCredentials :: Text -> DescribeChapCredentials
describeChapCredentials pTargetARN =
    DescribeChapCredentials'
    { _dccTargetARN = pTargetARN
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dccTargetARN :: Lens' DescribeChapCredentials Text
dccTargetARN = lens _dccTargetARN (\ s a -> s{_dccTargetARN = a});

instance AWSRequest DescribeChapCredentials where
        type Sv DescribeChapCredentials = StorageGateway
        type Rs DescribeChapCredentials =
             DescribeChapCredentialsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeChapCredentialsResponse' <$>
                   (x .?> "ChapCredentials" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeChapCredentials where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeChapCredentials" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeChapCredentials where
        toJSON DescribeChapCredentials'{..}
          = object ["TargetARN" .= _dccTargetARN]

instance ToPath DescribeChapCredentials where
        toPath = const "/"

instance ToQuery DescribeChapCredentials where
        toQuery = const mempty

-- | A JSON object containing a .
--
-- /See:/ 'describeChapCredentialsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrChapCredentials'
--
-- * 'dccrStatus'
data DescribeChapCredentialsResponse = DescribeChapCredentialsResponse'
    { _dccrChapCredentials :: !(Maybe [ChapInfo])
    , _dccrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeChapCredentialsResponse' smart constructor.
describeChapCredentialsResponse :: Int -> DescribeChapCredentialsResponse
describeChapCredentialsResponse pStatus =
    DescribeChapCredentialsResponse'
    { _dccrChapCredentials = Nothing
    , _dccrStatus = pStatus
    }

-- | An array of ChapInfo objects that represent CHAP credentials. Each
-- object in the array contains CHAP credential information for one
-- target-initiator pair. If no CHAP credentials are set, an empty array is
-- returned. CHAP credential information is provided in a JSON object with
-- the following fields:
--
-- -   __InitiatorName__: The iSCSI initiator that connects to the target.
--
-- -   __SecretToAuthenticateInitiator__: The secret key that the initiator
--     (for example, the Windows client) must provide to participate in
--     mutual CHAP with the target.
--
-- -   __SecretToAuthenticateTarget__: The secret key that the target must
--     provide to participate in mutual CHAP with the initiator (e.g.
--     Windows client).
--
-- -   __TargetARN__: The Amazon Resource Name (ARN) of the storage volume.
--
dccrChapCredentials :: Lens' DescribeChapCredentialsResponse [ChapInfo]
dccrChapCredentials = lens _dccrChapCredentials (\ s a -> s{_dccrChapCredentials = a}) . _Default;

-- | FIXME: Undocumented member.
dccrStatus :: Lens' DescribeChapCredentialsResponse Int
dccrStatus = lens _dccrStatus (\ s a -> s{_dccrStatus = a});
