{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StopConfigurationRecorder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Stops recording configurations of the AWS resources you have selected to
-- record in your AWS account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_StopConfigurationRecorder.html>
module Network.AWS.Config.StopConfigurationRecorder
    (
    -- * Request
      StopConfigurationRecorder
    -- ** Request constructor
    , stopConfigurationRecorder
    -- ** Request lenses
    , scrConfigurationRecorderName

    -- * Response
    , StopConfigurationRecorderResponse
    -- ** Response constructor
    , stopConfigurationRecorderResponse
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the StopConfigurationRecorder action.
--
-- /See:/ 'stopConfigurationRecorder' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scrConfigurationRecorderName'
newtype StopConfigurationRecorder = StopConfigurationRecorder'
    { _scrConfigurationRecorderName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopConfigurationRecorder' smart constructor.
stopConfigurationRecorder :: Text -> StopConfigurationRecorder
stopConfigurationRecorder pConfigurationRecorderName_ =
    StopConfigurationRecorder'
    { _scrConfigurationRecorderName = pConfigurationRecorderName_
    }

-- | The name of the recorder object that records each configuration change
-- made to the resources.
scrConfigurationRecorderName :: Lens' StopConfigurationRecorder Text
scrConfigurationRecorderName = lens _scrConfigurationRecorderName (\ s a -> s{_scrConfigurationRecorderName = a});

instance AWSRequest StopConfigurationRecorder where
        type Sv StopConfigurationRecorder = Config
        type Rs StopConfigurationRecorder =
             StopConfigurationRecorderResponse
        request = postJSON
        response
          = receiveNull StopConfigurationRecorderResponse'

instance ToHeaders StopConfigurationRecorder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.StopConfigurationRecorder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopConfigurationRecorder where
        toJSON StopConfigurationRecorder'{..}
          = object
              ["ConfigurationRecorderName" .=
                 _scrConfigurationRecorderName]

instance ToPath StopConfigurationRecorder where
        toPath = const mempty

instance ToQuery StopConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'stopConfigurationRecorderResponse' smart constructor.
data StopConfigurationRecorderResponse =
    StopConfigurationRecorderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopConfigurationRecorderResponse' smart constructor.
stopConfigurationRecorderResponse :: StopConfigurationRecorderResponse
stopConfigurationRecorderResponse = StopConfigurationRecorderResponse'
