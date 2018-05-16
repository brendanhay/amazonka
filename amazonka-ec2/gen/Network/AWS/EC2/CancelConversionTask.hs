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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active conversion task. The task can be the import of an instance or volume. The action removes all artifacts of the conversion, including a partially uploaded volume or instance. If the conversion is complete or is in the process of transferring the final disk image, the command fails and returns an exception.
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI> .
--
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

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CancelConversionTask.
--
--
--
-- /See:/ 'cancelConversionTask' smart constructor.
data CancelConversionTask = CancelConversionTask'
  { _cctReasonMessage    :: !(Maybe Text)
  , _cctDryRun           :: !(Maybe Bool)
  , _cctConversionTaskId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelConversionTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctReasonMessage' - The reason for canceling the conversion task.
--
-- * 'cctDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cctConversionTaskId' - The ID of the conversion task.
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
cctReasonMessage = lens _cctReasonMessage (\ s a -> s{_cctReasonMessage = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cctDryRun :: Lens' CancelConversionTask (Maybe Bool)
cctDryRun = lens _cctDryRun (\ s a -> s{_cctDryRun = a})

-- | The ID of the conversion task.
cctConversionTaskId :: Lens' CancelConversionTask Text
cctConversionTaskId = lens _cctConversionTaskId (\ s a -> s{_cctConversionTaskId = a})

instance AWSRequest CancelConversionTask where
        type Rs CancelConversionTask =
             CancelConversionTaskResponse
        request = postQuery ec2
        response = receiveNull CancelConversionTaskResponse'

instance Hashable CancelConversionTask where

instance NFData CancelConversionTask where

instance ToHeaders CancelConversionTask where
        toHeaders = const mempty

instance ToPath CancelConversionTask where
        toPath = const "/"

instance ToQuery CancelConversionTask where
        toQuery CancelConversionTask'{..}
          = mconcat
              ["Action" =: ("CancelConversionTask" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ReasonMessage" =: _cctReasonMessage,
               "DryRun" =: _cctDryRun,
               "ConversionTaskId" =: _cctConversionTaskId]

-- | /See:/ 'cancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse =
  CancelConversionTaskResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelConversionTaskResponse' with the minimum fields required to make a request.
--
cancelConversionTaskResponse
    :: CancelConversionTaskResponse
cancelConversionTaskResponse = CancelConversionTaskResponse'


instance NFData CancelConversionTaskResponse where
