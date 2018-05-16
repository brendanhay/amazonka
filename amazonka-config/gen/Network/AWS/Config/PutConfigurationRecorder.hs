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
-- Module      : Network.AWS.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration recorder to record the selected resource configurations.
--
--
-- You can use this action to change the role @roleARN@ or the @recordingGroup@ of an existing recorder. To change the role, call the action on the existing configuration recorder and specify a role.
--
module Network.AWS.Config.PutConfigurationRecorder
    (
    -- * Creating a Request
      putConfigurationRecorder
    , PutConfigurationRecorder
    -- * Request Lenses
    , pcrConfigurationRecorder

    -- * Destructuring the Response
    , putConfigurationRecorderResponse
    , PutConfigurationRecorderResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'PutConfigurationRecorder' action.
--
--
--
-- /See:/ 'putConfigurationRecorder' smart constructor.
newtype PutConfigurationRecorder = PutConfigurationRecorder'
  { _pcrConfigurationRecorder :: ConfigurationRecorder
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrConfigurationRecorder' - The configuration recorder object that records each configuration change made to the resources.
putConfigurationRecorder
    :: ConfigurationRecorder -- ^ 'pcrConfigurationRecorder'
    -> PutConfigurationRecorder
putConfigurationRecorder pConfigurationRecorder_ =
  PutConfigurationRecorder'
    {_pcrConfigurationRecorder = pConfigurationRecorder_}


-- | The configuration recorder object that records each configuration change made to the resources.
pcrConfigurationRecorder :: Lens' PutConfigurationRecorder ConfigurationRecorder
pcrConfigurationRecorder = lens _pcrConfigurationRecorder (\ s a -> s{_pcrConfigurationRecorder = a})

instance AWSRequest PutConfigurationRecorder where
        type Rs PutConfigurationRecorder =
             PutConfigurationRecorderResponse
        request = postJSON config
        response
          = receiveNull PutConfigurationRecorderResponse'

instance Hashable PutConfigurationRecorder where

instance NFData PutConfigurationRecorder where

instance ToHeaders PutConfigurationRecorder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.PutConfigurationRecorder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutConfigurationRecorder where
        toJSON PutConfigurationRecorder'{..}
          = object
              (catMaybes
                 [Just
                    ("ConfigurationRecorder" .=
                       _pcrConfigurationRecorder)])

instance ToPath PutConfigurationRecorder where
        toPath = const "/"

instance ToQuery PutConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'putConfigurationRecorderResponse' smart constructor.
data PutConfigurationRecorderResponse =
  PutConfigurationRecorderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutConfigurationRecorderResponse' with the minimum fields required to make a request.
--
putConfigurationRecorderResponse
    :: PutConfigurationRecorderResponse
putConfigurationRecorderResponse = PutConfigurationRecorderResponse'


instance NFData PutConfigurationRecorderResponse
         where
