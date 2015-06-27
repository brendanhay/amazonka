{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , dHAPGARN

    -- * Response
    , DescribeHAPGResponse
    -- ** Response constructor
    , describeHAPGResponse
    -- ** Response lenses
    , desState
    , desLastModifiedTimestamp
    , desHSMsPendingRegistration
    , desHAPGSerial
    , desHSMsPendingDeletion
    , desHSMsLastActionFailed
    , desPartitionSerialList
    , desHAPGARN
    , desLabel
    , desStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DescribeHapg action.
--
-- /See:/ 'describeHAPG' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dHAPGARN'
newtype DescribeHAPG = DescribeHAPG'
    { _dHAPGARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeHAPG' smart constructor.
describeHAPG :: Text -> DescribeHAPG
describeHAPG pHAPGARN =
    DescribeHAPG'
    { _dHAPGARN = pHAPGARN
    }

-- | The ARN of the high-availability partition group to describe.
dHAPGARN :: Lens' DescribeHAPG Text
dHAPGARN = lens _dHAPGARN (\ s a -> s{_dHAPGARN = a});

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
                     <*> (x .?> "Label")
                     <*> (pure (fromEnum s)))

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
          = object ["HapgArn" .= _dHAPGARN]

instance ToPath DescribeHAPG where
        toPath = const "/"

instance ToQuery DescribeHAPG where
        toQuery = const mempty

-- | Contains the output of the DescribeHapg action.
--
-- /See:/ 'describeHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desState'
--
-- * 'desLastModifiedTimestamp'
--
-- * 'desHSMsPendingRegistration'
--
-- * 'desHAPGSerial'
--
-- * 'desHSMsPendingDeletion'
--
-- * 'desHSMsLastActionFailed'
--
-- * 'desPartitionSerialList'
--
-- * 'desHAPGARN'
--
-- * 'desLabel'
--
-- * 'desStatus'
data DescribeHAPGResponse = DescribeHAPGResponse'
    { _desState                   :: Maybe CloudHSMObjectState
    , _desLastModifiedTimestamp   :: Maybe Text
    , _desHSMsPendingRegistration :: Maybe [Text]
    , _desHAPGSerial              :: Maybe Text
    , _desHSMsPendingDeletion     :: Maybe [Text]
    , _desHSMsLastActionFailed    :: Maybe [Text]
    , _desPartitionSerialList     :: Maybe [Text]
    , _desHAPGARN                 :: Maybe Text
    , _desLabel                   :: Maybe Text
    , _desStatus                  :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeHAPGResponse' smart constructor.
describeHAPGResponse :: Int -> DescribeHAPGResponse
describeHAPGResponse pStatus =
    DescribeHAPGResponse'
    { _desState = Nothing
    , _desLastModifiedTimestamp = Nothing
    , _desHSMsPendingRegistration = Nothing
    , _desHAPGSerial = Nothing
    , _desHSMsPendingDeletion = Nothing
    , _desHSMsLastActionFailed = Nothing
    , _desPartitionSerialList = Nothing
    , _desHAPGARN = Nothing
    , _desLabel = Nothing
    , _desStatus = pStatus
    }

-- | The state of the high-availability partition group.
desState :: Lens' DescribeHAPGResponse (Maybe CloudHSMObjectState)
desState = lens _desState (\ s a -> s{_desState = a});

-- | The date and time the high-availability partition group was last
-- modified.
desLastModifiedTimestamp :: Lens' DescribeHAPGResponse (Maybe Text)
desLastModifiedTimestamp = lens _desLastModifiedTimestamp (\ s a -> s{_desLastModifiedTimestamp = a});

-- | FIXME: Undocumented member.
desHSMsPendingRegistration :: Lens' DescribeHAPGResponse [Text]
desHSMsPendingRegistration = lens _desHSMsPendingRegistration (\ s a -> s{_desHSMsPendingRegistration = a}) . _Default;

-- | The serial number of the high-availability partition group.
desHAPGSerial :: Lens' DescribeHAPGResponse (Maybe Text)
desHAPGSerial = lens _desHAPGSerial (\ s a -> s{_desHAPGSerial = a});

-- | FIXME: Undocumented member.
desHSMsPendingDeletion :: Lens' DescribeHAPGResponse [Text]
desHSMsPendingDeletion = lens _desHSMsPendingDeletion (\ s a -> s{_desHSMsPendingDeletion = a}) . _Default;

-- | FIXME: Undocumented member.
desHSMsLastActionFailed :: Lens' DescribeHAPGResponse [Text]
desHSMsLastActionFailed = lens _desHSMsLastActionFailed (\ s a -> s{_desHSMsLastActionFailed = a}) . _Default;

-- | The list of partition serial numbers that belong to the
-- high-availability partition group.
desPartitionSerialList :: Lens' DescribeHAPGResponse [Text]
desPartitionSerialList = lens _desPartitionSerialList (\ s a -> s{_desPartitionSerialList = a}) . _Default;

-- | The ARN of the high-availability partition group.
desHAPGARN :: Lens' DescribeHAPGResponse (Maybe Text)
desHAPGARN = lens _desHAPGARN (\ s a -> s{_desHAPGARN = a});

-- | The label for the high-availability partition group.
desLabel :: Lens' DescribeHAPGResponse (Maybe Text)
desLabel = lens _desLabel (\ s a -> s{_desLabel = a});

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeHAPGResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
