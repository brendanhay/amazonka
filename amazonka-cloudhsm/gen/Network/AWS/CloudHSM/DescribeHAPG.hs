{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.DescribeHAPG
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
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHAPG.html>
module Network.AWS.CloudHSM.DescribeHAPG
    (
    -- * Request
      DescribeHAPG
    -- ** Request constructor
    , describeHAPG
    -- ** Request lenses
    , desHAPGARN

    -- * Response
    , DescribeHAPGResponse
    -- ** Response constructor
    , describeHAPGResponse
    -- ** Response lenses
    , dhrState
    , dhrLastModifiedTimestamp
    , dhrHSMsPendingRegistration
    , dhrHAPGSerial
    , dhrHSMsPendingDeletion
    , dhrHSMsLastActionFailed
    , dhrPartitionSerialList
    , dhrHAPGARN
    , dhrLabel
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeHAPG' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desHAPGARN'
newtype DescribeHAPG = DescribeHAPG'{_desHAPGARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeHAPG' smart constructor.
describeHAPG :: Text -> DescribeHAPG
describeHAPG pHAPGARN = DescribeHAPG'{_desHAPGARN = pHAPGARN};

-- | The ARN of the high-availability partition group to describe.
desHAPGARN :: Lens' DescribeHAPG Text
desHAPGARN = lens _desHAPGARN (\ s a -> s{_desHAPGARN = a});

instance AWSRequest DescribeHAPG where
        type Sv DescribeHAPG = CloudHSM
        type Rs DescribeHAPG = DescribeHAPGResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHAPGResponse' <$>
                   (x .?> "State") <*> (x .?> "LastModifiedTimestamp")
                     <*> (x .?> "HsmsPendingRegistration" .!@ mempty)
                     <*> (x .?> "HapgSerial")
                     <*> (x .?> "HsmsPendingDeletion" .!@ mempty)
                     <*> (x .?> "HsmsLastActionFailed" .!@ mempty)
                     <*> (x .?> "PartitionSerialList" .!@ mempty)
                     <*> (x .?> "HapgArn")
                     <*> (x .?> "Label"))

instance ToHeaders DescribeHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DescribeHAPG" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHAPG where
        toJSON DescribeHAPG'{..}
          = object ["HapgArn" .= _desHAPGARN]

instance ToPath DescribeHAPG where
        toPath = const "/"

instance ToQuery DescribeHAPG where
        toQuery = const mempty

-- | /See:/ 'describeHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrState'
--
-- * 'dhrLastModifiedTimestamp'
--
-- * 'dhrHSMsPendingRegistration'
--
-- * 'dhrHAPGSerial'
--
-- * 'dhrHSMsPendingDeletion'
--
-- * 'dhrHSMsLastActionFailed'
--
-- * 'dhrPartitionSerialList'
--
-- * 'dhrHAPGARN'
--
-- * 'dhrLabel'
data DescribeHAPGResponse = DescribeHAPGResponse'{_dhrState :: Maybe CloudHSMObjectState, _dhrLastModifiedTimestamp :: Maybe Text, _dhrHSMsPendingRegistration :: Maybe [Text], _dhrHAPGSerial :: Maybe Text, _dhrHSMsPendingDeletion :: Maybe [Text], _dhrHSMsLastActionFailed :: Maybe [Text], _dhrPartitionSerialList :: Maybe [Text], _dhrHAPGARN :: Maybe Text, _dhrLabel :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeHAPGResponse' smart constructor.
describeHAPGResponse :: DescribeHAPGResponse
describeHAPGResponse = DescribeHAPGResponse'{_dhrState = Nothing, _dhrLastModifiedTimestamp = Nothing, _dhrHSMsPendingRegistration = Nothing, _dhrHAPGSerial = Nothing, _dhrHSMsPendingDeletion = Nothing, _dhrHSMsLastActionFailed = Nothing, _dhrPartitionSerialList = Nothing, _dhrHAPGARN = Nothing, _dhrLabel = Nothing};

-- | The state of the high-availability partition group.
dhrState :: Lens' DescribeHAPGResponse (Maybe CloudHSMObjectState)
dhrState = lens _dhrState (\ s a -> s{_dhrState = a});

-- | The date and time the high-availability partition group was last
-- modified.
dhrLastModifiedTimestamp :: Lens' DescribeHAPGResponse (Maybe Text)
dhrLastModifiedTimestamp = lens _dhrLastModifiedTimestamp (\ s a -> s{_dhrLastModifiedTimestamp = a});

-- | FIXME: Undocumented member.
dhrHSMsPendingRegistration :: Lens' DescribeHAPGResponse [Text]
dhrHSMsPendingRegistration = lens _dhrHSMsPendingRegistration (\ s a -> s{_dhrHSMsPendingRegistration = a}) . _Default;

-- | The serial number of the high-availability partition group.
dhrHAPGSerial :: Lens' DescribeHAPGResponse (Maybe Text)
dhrHAPGSerial = lens _dhrHAPGSerial (\ s a -> s{_dhrHAPGSerial = a});

-- | FIXME: Undocumented member.
dhrHSMsPendingDeletion :: Lens' DescribeHAPGResponse [Text]
dhrHSMsPendingDeletion = lens _dhrHSMsPendingDeletion (\ s a -> s{_dhrHSMsPendingDeletion = a}) . _Default;

-- | FIXME: Undocumented member.
dhrHSMsLastActionFailed :: Lens' DescribeHAPGResponse [Text]
dhrHSMsLastActionFailed = lens _dhrHSMsLastActionFailed (\ s a -> s{_dhrHSMsLastActionFailed = a}) . _Default;

-- | The list of partition serial numbers that belong to the
-- high-availability partition group.
dhrPartitionSerialList :: Lens' DescribeHAPGResponse [Text]
dhrPartitionSerialList = lens _dhrPartitionSerialList (\ s a -> s{_dhrPartitionSerialList = a}) . _Default;

-- | The ARN of the high-availability partition group.
dhrHAPGARN :: Lens' DescribeHAPGResponse (Maybe Text)
dhrHAPGARN = lens _dhrHAPGARN (\ s a -> s{_dhrHAPGARN = a});

-- | The label for the high-availability partition group.
dhrLabel :: Lens' DescribeHAPGResponse (Maybe Text)
dhrLabel = lens _dhrLabel (\ s a -> s{_dhrLabel = a});
