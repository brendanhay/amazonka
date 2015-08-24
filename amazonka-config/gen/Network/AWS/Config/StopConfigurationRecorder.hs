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
-- Module      : Network.AWS.Config.StopConfigurationRecorder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops recording configurations of the AWS resources you have selected to
-- record in your AWS account.
--
-- /See:/ <http://docs.aws.amazon.com/config/latest/APIReference/API_StopConfigurationRecorder.html AWS API Reference> for StopConfigurationRecorder.
module Network.AWS.Config.StopConfigurationRecorder
    (
    -- * Creating a Request
      stopConfigurationRecorder
    , StopConfigurationRecorder
    -- * Request Lenses
    , scrConfigurationRecorderName

    -- * Destructuring the Response
    , stopConfigurationRecorderResponse
    , StopConfigurationRecorderResponse
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the StopConfigurationRecorder action.
--
-- /See:/ 'stopConfigurationRecorder' smart constructor.
newtype StopConfigurationRecorder = StopConfigurationRecorder'
    { _scrConfigurationRecorderName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrConfigurationRecorderName'
stopConfigurationRecorder
    :: Text -- ^ 'scrConfigurationRecorderName'
    -> StopConfigurationRecorder
stopConfigurationRecorder pConfigurationRecorderName_ =
    StopConfigurationRecorder'
    { _scrConfigurationRecorderName = pConfigurationRecorderName_
    }

-- | The name of the recorder object that records each configuration change
-- made to the resources.
scrConfigurationRecorderName :: Lens' StopConfigurationRecorder Text
scrConfigurationRecorderName = lens _scrConfigurationRecorderName (\ s a -> s{_scrConfigurationRecorderName = a});

instance AWSRequest StopConfigurationRecorder where
        type Rs StopConfigurationRecorder =
             StopConfigurationRecorderResponse
        request = postJSON config
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
              (catMaybes
                 [Just
                    ("ConfigurationRecorderName" .=
                       _scrConfigurationRecorderName)])

instance ToPath StopConfigurationRecorder where
        toPath = const "/"

instance ToQuery StopConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'stopConfigurationRecorderResponse' smart constructor.
data StopConfigurationRecorderResponse =
    StopConfigurationRecorderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopConfigurationRecorderResponse' with the minimum fields required to make a request.
--
stopConfigurationRecorderResponse
    :: StopConfigurationRecorderResponse
stopConfigurationRecorderResponse = StopConfigurationRecorderResponse'
