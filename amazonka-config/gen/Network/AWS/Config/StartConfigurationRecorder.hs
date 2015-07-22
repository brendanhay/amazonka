{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Starts recording configurations of the AWS resources you have selected
-- to record in your AWS account.
--
-- You must have created at least one delivery channel to successfully
-- start the configuration recorder.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_StartConfigurationRecorder.html>
module Network.AWS.Config.StartConfigurationRecorder
    (
    -- * Request
      StartConfigurationRecorder
    -- ** Request constructor
    , startConfigurationRecorder
    -- ** Request lenses
    , srqConfigurationRecorderName

    -- * Response
    , StartConfigurationRecorderResponse
    -- ** Response constructor
    , startConfigurationRecorderResponse
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the StartConfigurationRecorder action.
--
-- /See:/ 'startConfigurationRecorder' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srqConfigurationRecorderName'
newtype StartConfigurationRecorder = StartConfigurationRecorder'
    { _srqConfigurationRecorderName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartConfigurationRecorder' smart constructor.
startConfigurationRecorder :: Text -> StartConfigurationRecorder
startConfigurationRecorder pConfigurationRecorderName =
    StartConfigurationRecorder'
    { _srqConfigurationRecorderName = pConfigurationRecorderName
    }

-- | The name of the recorder object that records each configuration change
-- made to the resources.
srqConfigurationRecorderName :: Lens' StartConfigurationRecorder Text
srqConfigurationRecorderName = lens _srqConfigurationRecorderName (\ s a -> s{_srqConfigurationRecorderName = a});

instance AWSRequest StartConfigurationRecorder where
        type Sv StartConfigurationRecorder = Config
        type Rs StartConfigurationRecorder =
             StartConfigurationRecorderResponse
        request = postJSON
        response
          = receiveNull StartConfigurationRecorderResponse'

instance ToHeaders StartConfigurationRecorder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.StartConfigurationRecorder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartConfigurationRecorder where
        toJSON StartConfigurationRecorder'{..}
          = object
              ["ConfigurationRecorderName" .=
                 _srqConfigurationRecorderName]

instance ToPath StartConfigurationRecorder where
        toPath = const "/"

instance ToQuery StartConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'startConfigurationRecorderResponse' smart constructor.
data StartConfigurationRecorderResponse =
    StartConfigurationRecorderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartConfigurationRecorderResponse' smart constructor.
startConfigurationRecorderResponse :: StartConfigurationRecorderResponse
startConfigurationRecorderResponse = StartConfigurationRecorderResponse'
