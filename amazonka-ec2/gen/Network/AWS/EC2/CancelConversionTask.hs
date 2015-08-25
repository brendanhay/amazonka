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
-- Module      : Network.AWS.EC2.CancelConversionTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active conversion task. The task can be the import of an
-- instance or volume. The action removes all artifacts of the conversion,
-- including a partially uploaded volume or instance. If the conversion is
-- complete or is in the process of transferring the final disk image, the
-- command fails and returns an exception.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelConversionTask.html AWS API Reference> for CancelConversionTask.
module Network.AWS.EC2.CancelConversionTask
    (
    -- * Creating a Request
      cancelConversionTask
    , CancelConversionTask
    -- * Request Lenses
    , cctReasonMessage
    , cctDryRun
    , cctConversionTaskId

    -- * Destructuring the Response
    , cancelConversionTaskResponse
    , CancelConversionTaskResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cancelConversionTask' smart constructor.
data CancelConversionTask = CancelConversionTask'
    { _cctReasonMessage    :: !(Maybe Text)
    , _cctDryRun           :: !(Maybe Bool)
    , _cctConversionTaskId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelConversionTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctReasonMessage'
--
-- * 'cctDryRun'
--
-- * 'cctConversionTaskId'
cancelConversionTask
    :: Text -- ^ 'cctConversionTaskId'
    -> CancelConversionTask
cancelConversionTask pConversionTaskId_ =
    CancelConversionTask'
    { _cctReasonMessage = Nothing
    , _cctDryRun = Nothing
    , _cctConversionTaskId = pConversionTaskId_
    }

-- | The reason for canceling the conversion task.
cctReasonMessage :: Lens' CancelConversionTask (Maybe Text)
cctReasonMessage = lens _cctReasonMessage (\ s a -> s{_cctReasonMessage = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
cctDryRun :: Lens' CancelConversionTask (Maybe Bool)
cctDryRun = lens _cctDryRun (\ s a -> s{_cctDryRun = a});

-- | The ID of the conversion task.
cctConversionTaskId :: Lens' CancelConversionTask Text
cctConversionTaskId = lens _cctConversionTaskId (\ s a -> s{_cctConversionTaskId = a});

instance AWSRequest CancelConversionTask where
        type Rs CancelConversionTask =
             CancelConversionTaskResponse
        request = postQuery eC2
        response = receiveNull CancelConversionTaskResponse'

instance ToHeaders CancelConversionTask where
        toHeaders = const mempty

instance ToPath CancelConversionTask where
        toPath = const "/"

instance ToQuery CancelConversionTask where
        toQuery CancelConversionTask'{..}
          = mconcat
              ["Action" =: ("CancelConversionTask" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ReasonMessage" =: _cctReasonMessage,
               "DryRun" =: _cctDryRun,
               "ConversionTaskId" =: _cctConversionTaskId]

-- | /See:/ 'cancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse =
    CancelConversionTaskResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelConversionTaskResponse' with the minimum fields required to make a request.
--
cancelConversionTaskResponse
    :: CancelConversionTaskResponse
cancelConversionTaskResponse = CancelConversionTaskResponse'
