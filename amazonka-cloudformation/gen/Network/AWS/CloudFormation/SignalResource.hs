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
-- Module      : Network.AWS.CloudFormation.SignalResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to the specified resource with a success or failure status. You can use the SignalResource API in conjunction with a creation policy or update policy. AWS CloudFormation doesn't proceed with a stack creation or update until resources receive the required number of signals or the timeout period is exceeded. The SignalResource API is useful in cases where you want to send signals from anywhere other than an Amazon EC2 instance.
--
--
module Network.AWS.CloudFormation.SignalResource
    (
    -- * Creating a Request
      signalResource
    , SignalResource
    -- * Request Lenses
    , sigStackName
    , sigLogicalResourceId
    , sigUniqueId
    , sigStatus

    -- * Destructuring the Response
    , signalResourceResponse
    , SignalResourceResponse
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'SignalResource' action.
--
--
--
-- /See:/ 'signalResource' smart constructor.
data SignalResource = SignalResource'
  { _sigStackName         :: !Text
  , _sigLogicalResourceId :: !Text
  , _sigUniqueId          :: !Text
  , _sigStatus            :: !ResourceSignalStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SignalResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sigStackName' - The stack name or unique stack ID that includes the resource that you want to signal.
--
-- * 'sigLogicalResourceId' - The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
--
-- * 'sigUniqueId' - A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
--
-- * 'sigStatus' - The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
signalResource
    :: Text -- ^ 'sigStackName'
    -> Text -- ^ 'sigLogicalResourceId'
    -> Text -- ^ 'sigUniqueId'
    -> ResourceSignalStatus -- ^ 'sigStatus'
    -> SignalResource
signalResource pStackName_ pLogicalResourceId_ pUniqueId_ pStatus_ =
  SignalResource'
    { _sigStackName = pStackName_
    , _sigLogicalResourceId = pLogicalResourceId_
    , _sigUniqueId = pUniqueId_
    , _sigStatus = pStatus_
    }


-- | The stack name or unique stack ID that includes the resource that you want to signal.
sigStackName :: Lens' SignalResource Text
sigStackName = lens _sigStackName (\ s a -> s{_sigStackName = a})

-- | The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
sigLogicalResourceId :: Lens' SignalResource Text
sigLogicalResourceId = lens _sigLogicalResourceId (\ s a -> s{_sigLogicalResourceId = a})

-- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
sigUniqueId :: Lens' SignalResource Text
sigUniqueId = lens _sigUniqueId (\ s a -> s{_sigUniqueId = a})

-- | The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
sigStatus :: Lens' SignalResource ResourceSignalStatus
sigStatus = lens _sigStatus (\ s a -> s{_sigStatus = a})

instance AWSRequest SignalResource where
        type Rs SignalResource = SignalResourceResponse
        request = postQuery cloudFormation
        response = receiveNull SignalResourceResponse'

instance Hashable SignalResource where

instance NFData SignalResource where

instance ToHeaders SignalResource where
        toHeaders = const mempty

instance ToPath SignalResource where
        toPath = const "/"

instance ToQuery SignalResource where
        toQuery SignalResource'{..}
          = mconcat
              ["Action" =: ("SignalResource" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _sigStackName,
               "LogicalResourceId" =: _sigLogicalResourceId,
               "UniqueId" =: _sigUniqueId, "Status" =: _sigStatus]

-- | /See:/ 'signalResourceResponse' smart constructor.
data SignalResourceResponse =
  SignalResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SignalResourceResponse' with the minimum fields required to make a request.
--
signalResourceResponse
    :: SignalResourceResponse
signalResourceResponse = SignalResourceResponse'


instance NFData SignalResourceResponse where
