{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration recorder to record the selected resource
-- configurations.
--
-- You can use this action to change the role @roleARN@ and\/or the
-- @recordingGroup@ of an existing recorder. To change the role, call the
-- action on the existing configuration recorder and specify a role.
--
-- Currently, you can specify only one configuration recorder per account.
--
-- If @ConfigurationRecorder@ does not have the __recordingGroup__
-- parameter specified, the default is to record all supported resource
-- types.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_PutConfigurationRecorder.html>
module Network.AWS.Config.PutConfigurationRecorder
    (
    -- * Request
      PutConfigurationRecorder
    -- ** Request constructor
    , putConfigurationRecorder
    -- ** Request lenses
    , pcrConfigurationRecorder

    -- * Response
    , PutConfigurationRecorderResponse
    -- ** Response constructor
    , putConfigurationRecorderResponse
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the PutConfigurationRecorder action.
--
-- /See:/ 'putConfigurationRecorder' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcrConfigurationRecorder'
newtype PutConfigurationRecorder = PutConfigurationRecorder'
    { _pcrConfigurationRecorder :: ConfigurationRecorder
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutConfigurationRecorder' smart constructor.
putConfigurationRecorder :: ConfigurationRecorder -> PutConfigurationRecorder
putConfigurationRecorder pConfigurationRecorder_ =
    PutConfigurationRecorder'
    { _pcrConfigurationRecorder = pConfigurationRecorder_
    }

-- | The configuration recorder object that records each configuration change
-- made to the resources.
pcrConfigurationRecorder :: Lens' PutConfigurationRecorder ConfigurationRecorder
pcrConfigurationRecorder = lens _pcrConfigurationRecorder (\ s a -> s{_pcrConfigurationRecorder = a});

instance AWSRequest PutConfigurationRecorder where
        type Sv PutConfigurationRecorder = Config
        type Rs PutConfigurationRecorder =
             PutConfigurationRecorderResponse
        request = postJSON
        response
          = receiveNull PutConfigurationRecorderResponse'

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
              ["ConfigurationRecorder" .=
                 _pcrConfigurationRecorder]

instance ToPath PutConfigurationRecorder where
        toPath = const mempty

instance ToQuery PutConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'putConfigurationRecorderResponse' smart constructor.
data PutConfigurationRecorderResponse =
    PutConfigurationRecorderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutConfigurationRecorderResponse' smart constructor.
putConfigurationRecorderResponse :: PutConfigurationRecorderResponse
putConfigurationRecorderResponse = PutConfigurationRecorderResponse'
