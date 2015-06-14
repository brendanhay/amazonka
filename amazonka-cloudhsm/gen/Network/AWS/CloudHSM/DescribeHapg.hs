{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.DescribeHapg
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about a high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHapg.html>
module Network.AWS.CloudHSM.DescribeHapg
    (
    -- * Request
      DescribeHapg
    -- ** Request constructor
    , describeHapg
    -- ** Request lenses
    , desHapgARN

    -- * Response
    , DescribeHapgResponse
    -- ** Response constructor
    , describeHapgResponse
    -- ** Response lenses
    , dhrState
    , dhrLastModifiedTimestamp
    , dhrHSMsPendingRegistration
    , dhrHapgSerial
    , dhrHSMsPendingDeletion
    , dhrHSMsLastActionFailed
    , dhrPartitionSerialList
    , dhrHapgARN
    , dhrLabel
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'describeHapg' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desHapgARN'
newtype DescribeHapg = DescribeHapg'{_desHapgARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeHapg' smart constructor.
describeHapg :: Text -> DescribeHapg
describeHapg pHapgARN = DescribeHapg'{_desHapgARN = pHapgARN};

-- | The ARN of the high-availability partition group to describe.
desHapgARN :: Lens' DescribeHapg Text
desHapgARN = lens _desHapgARN (\ s a -> s{_desHapgARN = a});

instance AWSRequest DescribeHapg where
        type Sv DescribeHapg = CloudHSM
        type Rs DescribeHapg = DescribeHapgResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHapgResponse' <$>
                   x .?> "State" <*> x .?> "LastModifiedTimestamp" <*>
                     x .?> "HsmsPendingRegistration" .!@ mempty
                     <*> x .?> "HapgSerial"
                     <*> x .?> "HsmsPendingDeletion" .!@ mempty
                     <*> x .?> "HsmsLastActionFailed" .!@ mempty
                     <*> x .?> "PartitionSerialList" .!@ mempty
                     <*> x .?> "HapgArn"
                     <*> x .?> "Label")

instance ToHeaders DescribeHapg where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DescribeHapg" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHapg where
        toJSON DescribeHapg'{..}
          = object ["HapgArn" .= _desHapgARN]

instance ToPath DescribeHapg where
        toPath = const "/"

instance ToQuery DescribeHapg where
        toQuery = const mempty

-- | /See:/ 'describeHapgResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrState'
--
-- * 'dhrLastModifiedTimestamp'
--
-- * 'dhrHSMsPendingRegistration'
--
-- * 'dhrHapgSerial'
--
-- * 'dhrHSMsPendingDeletion'
--
-- * 'dhrHSMsLastActionFailed'
--
-- * 'dhrPartitionSerialList'
--
-- * 'dhrHapgARN'
--
-- * 'dhrLabel'
data DescribeHapgResponse = DescribeHapgResponse'{_dhrState :: Maybe CloudHSMObjectState, _dhrLastModifiedTimestamp :: Maybe Text, _dhrHSMsPendingRegistration :: [Text], _dhrHapgSerial :: Maybe Text, _dhrHSMsPendingDeletion :: [Text], _dhrHSMsLastActionFailed :: [Text], _dhrPartitionSerialList :: [Text], _dhrHapgARN :: Maybe Text, _dhrLabel :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeHapgResponse' smart constructor.
describeHapgResponse :: DescribeHapgResponse
describeHapgResponse = DescribeHapgResponse'{_dhrState = Nothing, _dhrLastModifiedTimestamp = Nothing, _dhrHSMsPendingRegistration = mempty, _dhrHapgSerial = Nothing, _dhrHSMsPendingDeletion = mempty, _dhrHSMsLastActionFailed = mempty, _dhrPartitionSerialList = mempty, _dhrHapgARN = Nothing, _dhrLabel = Nothing};

-- | The state of the high-availability partition group.
dhrState :: Lens' DescribeHapgResponse (Maybe CloudHSMObjectState)
dhrState = lens _dhrState (\ s a -> s{_dhrState = a});

-- | The date and time the high-availability partition group was last
-- modified.
dhrLastModifiedTimestamp :: Lens' DescribeHapgResponse (Maybe Text)
dhrLastModifiedTimestamp = lens _dhrLastModifiedTimestamp (\ s a -> s{_dhrLastModifiedTimestamp = a});

-- | FIXME: Undocumented member.
dhrHSMsPendingRegistration :: Lens' DescribeHapgResponse [Text]
dhrHSMsPendingRegistration = lens _dhrHSMsPendingRegistration (\ s a -> s{_dhrHSMsPendingRegistration = a});

-- | The serial number of the high-availability partition group.
dhrHapgSerial :: Lens' DescribeHapgResponse (Maybe Text)
dhrHapgSerial = lens _dhrHapgSerial (\ s a -> s{_dhrHapgSerial = a});

-- | FIXME: Undocumented member.
dhrHSMsPendingDeletion :: Lens' DescribeHapgResponse [Text]
dhrHSMsPendingDeletion = lens _dhrHSMsPendingDeletion (\ s a -> s{_dhrHSMsPendingDeletion = a});

-- | FIXME: Undocumented member.
dhrHSMsLastActionFailed :: Lens' DescribeHapgResponse [Text]
dhrHSMsLastActionFailed = lens _dhrHSMsLastActionFailed (\ s a -> s{_dhrHSMsLastActionFailed = a});

-- | The list of partition serial numbers that belong to the
-- high-availability partition group.
dhrPartitionSerialList :: Lens' DescribeHapgResponse [Text]
dhrPartitionSerialList = lens _dhrPartitionSerialList (\ s a -> s{_dhrPartitionSerialList = a});

-- | The ARN of the high-availability partition group.
dhrHapgARN :: Lens' DescribeHapgResponse (Maybe Text)
dhrHapgARN = lens _dhrHapgARN (\ s a -> s{_dhrHapgARN = a});

-- | The label for the high-availability partition group.
dhrLabel :: Lens' DescribeHapgResponse (Maybe Text)
dhrLabel = lens _dhrLabel (\ s a -> s{_dhrLabel = a});
