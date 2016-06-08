{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHAPG
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a high-availability partition group.
module Network.AWS.CloudHSM.DescribeHAPG
    (
    -- * Creating a Request
      describeHAPG
    , DescribeHAPG
    -- * Request Lenses
    , dhapgHAPGARN

    -- * Destructuring the Response
    , describeHAPGResponse
    , DescribeHAPGResponse
    -- * Response Lenses
    , dhapgrsState
    , dhapgrsLastModifiedTimestamp
    , dhapgrsHSMsPendingRegistration
    , dhapgrsHSMsPendingDeletion
    , dhapgrsHAPGSerial
    , dhapgrsHSMsLastActionFailed
    , dhapgrsPartitionSerialList
    , dhapgrsHAPGARN
    , dhapgrsLabel
    , dhapgrsResponseStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the < DescribeHapg> action.
--
-- /See:/ 'describeHAPG' smart constructor.
newtype DescribeHAPG = DescribeHAPG'
    { _dhapgHAPGARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeHAPG' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhapgHAPGARN'
describeHAPG
    :: Text -- ^ 'dhapgHAPGARN'
    -> DescribeHAPG
describeHAPG pHAPGARN_ =
    DescribeHAPG'
    { _dhapgHAPGARN = pHAPGARN_
    }

-- | The ARN of the high-availability partition group to describe.
dhapgHAPGARN :: Lens' DescribeHAPG Text
dhapgHAPGARN = lens _dhapgHAPGARN (\ s a -> s{_dhapgHAPGARN = a});

instance AWSRequest DescribeHAPG where
        type Rs DescribeHAPG = DescribeHAPGResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHAPGResponse' <$>
                   (x .?> "State") <*> (x .?> "LastModifiedTimestamp")
                     <*> (x .?> "HsmsPendingRegistration" .!@ mempty)
                     <*> (x .?> "HsmsPendingDeletion" .!@ mempty)
                     <*> (x .?> "HapgSerial")
                     <*> (x .?> "HsmsLastActionFailed" .!@ mempty)
                     <*> (x .?> "PartitionSerialList" .!@ mempty)
                     <*> (x .?> "HapgArn")
                     <*> (x .?> "Label")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeHAPG

instance NFData DescribeHAPG

instance ToHeaders DescribeHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DescribeHapg" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHAPG where
        toJSON DescribeHAPG'{..}
          = object
              (catMaybes [Just ("HapgArn" .= _dhapgHAPGARN)])

instance ToPath DescribeHAPG where
        toPath = const "/"

instance ToQuery DescribeHAPG where
        toQuery = const mempty

-- | Contains the output of the < DescribeHapg> action.
--
-- /See:/ 'describeHAPGResponse' smart constructor.
data DescribeHAPGResponse = DescribeHAPGResponse'
    { _dhapgrsState                   :: !(Maybe CloudHSMObjectState)
    , _dhapgrsLastModifiedTimestamp   :: !(Maybe Text)
    , _dhapgrsHSMsPendingRegistration :: !(Maybe [Text])
    , _dhapgrsHSMsPendingDeletion     :: !(Maybe [Text])
    , _dhapgrsHAPGSerial              :: !(Maybe Text)
    , _dhapgrsHSMsLastActionFailed    :: !(Maybe [Text])
    , _dhapgrsPartitionSerialList     :: !(Maybe [Text])
    , _dhapgrsHAPGARN                 :: !(Maybe Text)
    , _dhapgrsLabel                   :: !(Maybe Text)
    , _dhapgrsResponseStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeHAPGResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhapgrsState'
--
-- * 'dhapgrsLastModifiedTimestamp'
--
-- * 'dhapgrsHSMsPendingRegistration'
--
-- * 'dhapgrsHSMsPendingDeletion'
--
-- * 'dhapgrsHAPGSerial'
--
-- * 'dhapgrsHSMsLastActionFailed'
--
-- * 'dhapgrsPartitionSerialList'
--
-- * 'dhapgrsHAPGARN'
--
-- * 'dhapgrsLabel'
--
-- * 'dhapgrsResponseStatus'
describeHAPGResponse
    :: Int -- ^ 'dhapgrsResponseStatus'
    -> DescribeHAPGResponse
describeHAPGResponse pResponseStatus_ =
    DescribeHAPGResponse'
    { _dhapgrsState = Nothing
    , _dhapgrsLastModifiedTimestamp = Nothing
    , _dhapgrsHSMsPendingRegistration = Nothing
    , _dhapgrsHSMsPendingDeletion = Nothing
    , _dhapgrsHAPGSerial = Nothing
    , _dhapgrsHSMsLastActionFailed = Nothing
    , _dhapgrsPartitionSerialList = Nothing
    , _dhapgrsHAPGARN = Nothing
    , _dhapgrsLabel = Nothing
    , _dhapgrsResponseStatus = pResponseStatus_
    }

-- | The state of the high-availability partition group.
dhapgrsState :: Lens' DescribeHAPGResponse (Maybe CloudHSMObjectState)
dhapgrsState = lens _dhapgrsState (\ s a -> s{_dhapgrsState = a});

-- | The date and time the high-availability partition group was last modified.
dhapgrsLastModifiedTimestamp :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsLastModifiedTimestamp = lens _dhapgrsLastModifiedTimestamp (\ s a -> s{_dhapgrsLastModifiedTimestamp = a});

-- | Undocumented member.
dhapgrsHSMsPendingRegistration :: Lens' DescribeHAPGResponse [Text]
dhapgrsHSMsPendingRegistration = lens _dhapgrsHSMsPendingRegistration (\ s a -> s{_dhapgrsHSMsPendingRegistration = a}) . _Default . _Coerce;

-- | Undocumented member.
dhapgrsHSMsPendingDeletion :: Lens' DescribeHAPGResponse [Text]
dhapgrsHSMsPendingDeletion = lens _dhapgrsHSMsPendingDeletion (\ s a -> s{_dhapgrsHSMsPendingDeletion = a}) . _Default . _Coerce;

-- | The serial number of the high-availability partition group.
dhapgrsHAPGSerial :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsHAPGSerial = lens _dhapgrsHAPGSerial (\ s a -> s{_dhapgrsHAPGSerial = a});

-- | Undocumented member.
dhapgrsHSMsLastActionFailed :: Lens' DescribeHAPGResponse [Text]
dhapgrsHSMsLastActionFailed = lens _dhapgrsHSMsLastActionFailed (\ s a -> s{_dhapgrsHSMsLastActionFailed = a}) . _Default . _Coerce;

-- | The list of partition serial numbers that belong to the high-availability partition group.
dhapgrsPartitionSerialList :: Lens' DescribeHAPGResponse [Text]
dhapgrsPartitionSerialList = lens _dhapgrsPartitionSerialList (\ s a -> s{_dhapgrsPartitionSerialList = a}) . _Default . _Coerce;

-- | The ARN of the high-availability partition group.
dhapgrsHAPGARN :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsHAPGARN = lens _dhapgrsHAPGARN (\ s a -> s{_dhapgrsHAPGARN = a});

-- | The label for the high-availability partition group.
dhapgrsLabel :: Lens' DescribeHAPGResponse (Maybe Text)
dhapgrsLabel = lens _dhapgrsLabel (\ s a -> s{_dhapgrsLabel = a});

-- | The response status code.
dhapgrsResponseStatus :: Lens' DescribeHAPGResponse Int
dhapgrsResponseStatus = lens _dhapgrsResponseStatus (\ s a -> s{_dhapgrsResponseStatus = a});

instance NFData DescribeHAPGResponse
