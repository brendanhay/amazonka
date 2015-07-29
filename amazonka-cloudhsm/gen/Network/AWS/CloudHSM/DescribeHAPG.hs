{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHAPG.html>
module Network.AWS.CloudHSM.DescribeHAPG
    (
    -- * Request
      DescribeHAPG
    -- ** Request constructor
    , describeHAPG
    -- ** Request lenses
    , dhapgHAPGARN

    -- * Response
    , DescribeHAPGResponse
    -- ** Response constructor
    , describeHAPGResponse
    -- ** Response lenses
    , dhapgrsState
    , dhapgrsLastModifiedTimestamp
    , dhapgrsHSMsPendingRegistration
    , dhapgrsHAPGSerial
    , dhapgrsHSMsPendingDeletion
    , dhapgrsHSMsLastActionFailed
    , dhapgrsPartitionSerialList
    , dhapgrsHAPGARN
    , dhapgrsLabel
    , dhapgrsStatus
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
-- * 'dhapgHAPGARN'
newtype DescribeHAPG = DescribeHAPG'
    { _dhapgHAPGARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHAPG' smart constructor.
describeHAPG :: Text -> DescribeHAPG
describeHAPG pHAPGARN_ =
    DescribeHAPG'
    { _dhapgHAPGARN = pHAPGARN_
    }

-- | The ARN of the high-availability partition group to describe.
dhapgHAPGARN :: Lens' DescribeHAPG Text
dhapgHAPGARN = lens _dhapgHAPGARN (\ s a -> s{_dhapgHAPGARN = a});

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
          = object ["HapgArn" .= _dhapgHAPGARN]

instance ToPath DescribeHAPG where
        toPath = const mempty

instance ToQuery DescribeHAPG where
        toQuery = const mempty

-- | Contains the output of the DescribeHapg action.
--
-- /See:/ 'describeHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhapgrsState'
--
-- * 'dhapgrsLastModifiedTimestamp'
--
-- * 'dhapgrsHSMsPendingRegistration'
--
-- * 'dhapgrsHAPGSerial'
--
-- * 'dhapgrsHSMsPendingDeletion'
--
-- * 'dhapgrsHSMsLastActionFailed'
--
-- * 'dhapgrsPartitionSerialList'
--
-- * 'dhapgrsHAPGARN'
--
-- * 'dhapgrsLabel'
--
-- * 'dhapgrsStatus'
data DescribeHAPGResponse = DescribeHAPGResponse'
    { _dhapgrsState                   :: !(Maybe CloudHSMObjectState)
    , _dhapgrsLastModifiedTimestamp   :: !(Maybe Text)
    , _dhapgrsHSMsPendingRegistration :: !(Maybe [Text])
    , _dhapgrsHAPGSerial              :: !(Maybe Text)
    , _dhapgrsHSMsPendingDeletion     :: !(Maybe [Text])
    , _dhapgrsHSMsLastActionFailed    :: !(Maybe [Text])
    , _dhapgrsPartitionSerialList     :: !(Maybe [Text])
    , _dhapgrsHAPGARN                 :: !(Maybe Text)
    , _dhapgrsLabel                   :: !(Maybe Text)
    , _dhapgrsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHAPGResponse' smart constructor.
describeHAPGResponse :: Int -> DescribeHAPGResponse
describeHAPGResponse pStatus_ =
    DescribeHAPGResponse'
    { _dhapgrsState = Nothing
    , _dhapgrsLastModifiedTimestamp = Nothing
    , _dhapgrsHSMsPendingRegistration = Nothing
    , _dhapgrsHAPGSerial = Nothing
    , _dhapgrsHSMsPendingDeletion = Nothing
    , _dhapgrsHSMsLastActionFailed = Nothing
    , _dhapgrsPartitionSerialList = Nothing
    , _dhapgrsHAPGARN = Nothing
    , _dhapgrsLabel = Nothing
    , _dhapgrsStatus = pStatus_
    }

-- | The state of the high-availability partition group.
dhapgrsState :: Lens' DescribeHAPGResponse (Maybe CloudHSMObjectState)
dhapgrsState = lens _dhapgrsState (\ s a -> s{_dhapgrsState = a});

-- | The date and time the high-availability partition group was last
-- modified.
dhapgrsLastModifiedTimestamp :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsLastModifiedTimestamp = lens _dhapgrsLastModifiedTimestamp (\ s a -> s{_dhapgrsLastModifiedTimestamp = a});

-- | FIXME: Undocumented member.
dhapgrsHSMsPendingRegistration :: Lens' DescribeHAPGResponse [Text]
dhapgrsHSMsPendingRegistration = lens _dhapgrsHSMsPendingRegistration (\ s a -> s{_dhapgrsHSMsPendingRegistration = a}) . _Default . _Coerce;

-- | The serial number of the high-availability partition group.
dhapgrsHAPGSerial :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsHAPGSerial = lens _dhapgrsHAPGSerial (\ s a -> s{_dhapgrsHAPGSerial = a});

-- | FIXME: Undocumented member.
dhapgrsHSMsPendingDeletion :: Lens' DescribeHAPGResponse [Text]
dhapgrsHSMsPendingDeletion = lens _dhapgrsHSMsPendingDeletion (\ s a -> s{_dhapgrsHSMsPendingDeletion = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
dhapgrsHSMsLastActionFailed :: Lens' DescribeHAPGResponse [Text]
dhapgrsHSMsLastActionFailed = lens _dhapgrsHSMsLastActionFailed (\ s a -> s{_dhapgrsHSMsLastActionFailed = a}) . _Default . _Coerce;

-- | The list of partition serial numbers that belong to the
-- high-availability partition group.
dhapgrsPartitionSerialList :: Lens' DescribeHAPGResponse [Text]
dhapgrsPartitionSerialList = lens _dhapgrsPartitionSerialList (\ s a -> s{_dhapgrsPartitionSerialList = a}) . _Default . _Coerce;

-- | The ARN of the high-availability partition group.
dhapgrsHAPGARN :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsHAPGARN = lens _dhapgrsHAPGARN (\ s a -> s{_dhapgrsHAPGARN = a});

-- | The label for the high-availability partition group.
dhapgrsLabel :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsLabel = lens _dhapgrsLabel (\ s a -> s{_dhapgrsLabel = a});

-- | FIXME: Undocumented member.
dhapgrsStatus :: Lens' DescribeHAPGResponse Int
dhapgrsStatus = lens _dhapgrsStatus (\ s a -> s{_dhapgrsStatus = a});
