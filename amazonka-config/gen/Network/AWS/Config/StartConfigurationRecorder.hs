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
-- Module      : Network.AWS.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts recording configurations of the AWS resources you have selected to record in your AWS account.
--
--
-- You must have created at least one delivery channel to successfully start the configuration recorder.
--
module Network.AWS.Config.StartConfigurationRecorder
    (
    -- * Creating a Request
      startConfigurationRecorder
    , StartConfigurationRecorder
    -- * Request Lenses
    , sConfigurationRecorderName

    -- * Destructuring the Response
    , startConfigurationRecorderResponse
    , StartConfigurationRecorderResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'StartConfigurationRecorder' action.
--
--
--
-- /See:/ 'startConfigurationRecorder' smart constructor.
newtype StartConfigurationRecorder = StartConfigurationRecorder'
  { _sConfigurationRecorderName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sConfigurationRecorderName' - The name of the recorder object that records each configuration change made to the resources.
startConfigurationRecorder
    :: Text -- ^ 'sConfigurationRecorderName'
    -> StartConfigurationRecorder
startConfigurationRecorder pConfigurationRecorderName_ =
  StartConfigurationRecorder'
    {_sConfigurationRecorderName = pConfigurationRecorderName_}


-- | The name of the recorder object that records each configuration change made to the resources.
sConfigurationRecorderName :: Lens' StartConfigurationRecorder Text
sConfigurationRecorderName = lens _sConfigurationRecorderName (\ s a -> s{_sConfigurationRecorderName = a})

instance AWSRequest StartConfigurationRecorder where
        type Rs StartConfigurationRecorder =
             StartConfigurationRecorderResponse
        request = postJSON config
        response
          = receiveNull StartConfigurationRecorderResponse'

instance Hashable StartConfigurationRecorder where

instance NFData StartConfigurationRecorder where

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
              (catMaybes
                 [Just
                    ("ConfigurationRecorderName" .=
                       _sConfigurationRecorderName)])

instance ToPath StartConfigurationRecorder where
        toPath = const "/"

instance ToQuery StartConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'startConfigurationRecorderResponse' smart constructor.
data StartConfigurationRecorderResponse =
  StartConfigurationRecorderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartConfigurationRecorderResponse' with the minimum fields required to make a request.
--
startConfigurationRecorderResponse
    :: StartConfigurationRecorderResponse
startConfigurationRecorderResponse = StartConfigurationRecorderResponse'


instance NFData StartConfigurationRecorderResponse
         where
