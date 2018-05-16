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
-- Module      : Network.AWS.Config.DeleteConfigurationRecorder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration recorder.
--
--
-- After the configuration recorder is deleted, AWS Config will not record resource configuration changes until you create a new configuration recorder.
--
-- This action does not delete the configuration information that was previously recorded. You will be able to access the previously recorded information by using the @GetResourceConfigHistory@ action, but you will not be able to access this information in the AWS Config console until you create a new configuration recorder.
--
module Network.AWS.Config.DeleteConfigurationRecorder
    (
    -- * Creating a Request
      deleteConfigurationRecorder
    , DeleteConfigurationRecorder
    -- * Request Lenses
    , dcrConfigurationRecorderName

    -- * Destructuring the Response
    , deleteConfigurationRecorderResponse
    , DeleteConfigurationRecorderResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request object for the @DeleteConfigurationRecorder@ action.
--
--
--
-- /See:/ 'deleteConfigurationRecorder' smart constructor.
newtype DeleteConfigurationRecorder = DeleteConfigurationRecorder'
  { _dcrConfigurationRecorderName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrConfigurationRecorderName' - The name of the configuration recorder to be deleted. You can retrieve the name of your configuration recorder by using the @DescribeConfigurationRecorders@ action.
deleteConfigurationRecorder
    :: Text -- ^ 'dcrConfigurationRecorderName'
    -> DeleteConfigurationRecorder
deleteConfigurationRecorder pConfigurationRecorderName_ =
  DeleteConfigurationRecorder'
    {_dcrConfigurationRecorderName = pConfigurationRecorderName_}


-- | The name of the configuration recorder to be deleted. You can retrieve the name of your configuration recorder by using the @DescribeConfigurationRecorders@ action.
dcrConfigurationRecorderName :: Lens' DeleteConfigurationRecorder Text
dcrConfigurationRecorderName = lens _dcrConfigurationRecorderName (\ s a -> s{_dcrConfigurationRecorderName = a})

instance AWSRequest DeleteConfigurationRecorder where
        type Rs DeleteConfigurationRecorder =
             DeleteConfigurationRecorderResponse
        request = postJSON config
        response
          = receiveNull DeleteConfigurationRecorderResponse'

instance Hashable DeleteConfigurationRecorder where

instance NFData DeleteConfigurationRecorder where

instance ToHeaders DeleteConfigurationRecorder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteConfigurationRecorder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteConfigurationRecorder where
        toJSON DeleteConfigurationRecorder'{..}
          = object
              (catMaybes
                 [Just
                    ("ConfigurationRecorderName" .=
                       _dcrConfigurationRecorderName)])

instance ToPath DeleteConfigurationRecorder where
        toPath = const "/"

instance ToQuery DeleteConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'deleteConfigurationRecorderResponse' smart constructor.
data DeleteConfigurationRecorderResponse =
  DeleteConfigurationRecorderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationRecorderResponse' with the minimum fields required to make a request.
--
deleteConfigurationRecorderResponse
    :: DeleteConfigurationRecorderResponse
deleteConfigurationRecorderResponse = DeleteConfigurationRecorderResponse'


instance NFData DeleteConfigurationRecorderResponse
         where
