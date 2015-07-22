{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SignalResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to the specified resource with a success or failure
-- status. You can use the SignalResource API in conjunction with a
-- creation policy or update policy. AWS CloudFormation doesn\'t proceed
-- with a stack creation or update until resources receive the required
-- number of signals or the timeout period is exceeded. The SignalResource
-- API is useful in cases where you want to send signals from anywhere
-- other than an Amazon EC2 instance.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SignalResource.html>
module Network.AWS.CloudFormation.SignalResource
    (
    -- * Request
      SignalResource
    -- ** Request constructor
    , signalResource
    -- ** Request lenses
    , srrqStackName
    , srrqLogicalResourceId
    , srrqUniqueId
    , srrqStatus

    -- * Response
    , SignalResourceResponse
    -- ** Response constructor
    , signalResourceResponse
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the SignalResource action.
--
-- /See:/ 'signalResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srrqStackName'
--
-- * 'srrqLogicalResourceId'
--
-- * 'srrqUniqueId'
--
-- * 'srrqStatus'
data SignalResource = SignalResource'
    { _srrqStackName         :: !Text
    , _srrqLogicalResourceId :: !Text
    , _srrqUniqueId          :: !Text
    , _srrqStatus            :: !ResourceSignalStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SignalResource' smart constructor.
signalResource :: Text -> Text -> Text -> ResourceSignalStatus -> SignalResource
signalResource pStackName pLogicalResourceId pUniqueId pStatus =
    SignalResource'
    { _srrqStackName = pStackName
    , _srrqLogicalResourceId = pLogicalResourceId
    , _srrqUniqueId = pUniqueId
    , _srrqStatus = pStatus
    }

-- | The stack name or unique stack ID that includes the resource that you
-- want to signal.
srrqStackName :: Lens' SignalResource Text
srrqStackName = lens _srrqStackName (\ s a -> s{_srrqStackName = a});

-- | The logical ID of the resource that you want to signal. The logical ID
-- is the name of the resource that given in the template.
srrqLogicalResourceId :: Lens' SignalResource Text
srrqLogicalResourceId = lens _srrqLogicalResourceId (\ s a -> s{_srrqLogicalResourceId = a});

-- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto
-- Scaling groups, specify the instance ID that you are signaling as the
-- unique ID. If you send multiple signals to a single resource (such as
-- signaling a wait condition), each signal requires a different unique ID.
srrqUniqueId :: Lens' SignalResource Text
srrqUniqueId = lens _srrqUniqueId (\ s a -> s{_srrqUniqueId = a});

-- | The status of the signal, which is either success or failure. A failure
-- signal causes AWS CloudFormation to immediately fail the stack creation
-- or update.
srrqStatus :: Lens' SignalResource ResourceSignalStatus
srrqStatus = lens _srrqStatus (\ s a -> s{_srrqStatus = a});

instance AWSRequest SignalResource where
        type Sv SignalResource = CloudFormation
        type Rs SignalResource = SignalResourceResponse
        request = post
        response = receiveNull SignalResourceResponse'

instance ToHeaders SignalResource where
        toHeaders = const mempty

instance ToPath SignalResource where
        toPath = const "/"

instance ToQuery SignalResource where
        toQuery SignalResource'{..}
          = mconcat
              ["Action" =: ("SignalResource" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _srrqStackName,
               "LogicalResourceId" =: _srrqLogicalResourceId,
               "UniqueId" =: _srrqUniqueId, "Status" =: _srrqStatus]

-- | /See:/ 'signalResourceResponse' smart constructor.
data SignalResourceResponse =
    SignalResourceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SignalResourceResponse' smart constructor.
signalResourceResponse :: SignalResourceResponse
signalResourceResponse = SignalResourceResponse'
