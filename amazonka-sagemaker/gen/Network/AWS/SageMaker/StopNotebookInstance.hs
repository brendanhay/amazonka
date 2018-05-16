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
-- Module      : Network.AWS.SageMaker.StopNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the ML compute instance. Before terminating the instance, Amazon SageMaker disconnects the ML storage volume from it. Amazon SageMaker preserves the ML storage volume.
--
--
-- To access data on the ML storage volume for a notebook instance that has been terminated, call the @StartNotebookInstance@ API. @StartNotebookInstance@ launches another ML compute instance, configures it, and attaches the preserved ML storage volume so you can continue your work.
--
module Network.AWS.SageMaker.StopNotebookInstance
    (
    -- * Creating a Request
      stopNotebookInstance
    , StopNotebookInstance
    -- * Request Lenses
    , sniNotebookInstanceName

    -- * Destructuring the Response
    , stopNotebookInstanceResponse
    , StopNotebookInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'stopNotebookInstance' smart constructor.
newtype StopNotebookInstance = StopNotebookInstance'
  { _sniNotebookInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sniNotebookInstanceName' - The name of the notebook instance to terminate.
stopNotebookInstance
    :: Text -- ^ 'sniNotebookInstanceName'
    -> StopNotebookInstance
stopNotebookInstance pNotebookInstanceName_ =
  StopNotebookInstance' {_sniNotebookInstanceName = pNotebookInstanceName_}


-- | The name of the notebook instance to terminate.
sniNotebookInstanceName :: Lens' StopNotebookInstance Text
sniNotebookInstanceName = lens _sniNotebookInstanceName (\ s a -> s{_sniNotebookInstanceName = a})

instance AWSRequest StopNotebookInstance where
        type Rs StopNotebookInstance =
             StopNotebookInstanceResponse
        request = postJSON sageMaker
        response = receiveNull StopNotebookInstanceResponse'

instance Hashable StopNotebookInstance where

instance NFData StopNotebookInstance where

instance ToHeaders StopNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.StopNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopNotebookInstance where
        toJSON StopNotebookInstance'{..}
          = object
              (catMaybes
                 [Just
                    ("NotebookInstanceName" .=
                       _sniNotebookInstanceName)])

instance ToPath StopNotebookInstance where
        toPath = const "/"

instance ToQuery StopNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'stopNotebookInstanceResponse' smart constructor.
data StopNotebookInstanceResponse =
  StopNotebookInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopNotebookInstanceResponse' with the minimum fields required to make a request.
--
stopNotebookInstanceResponse
    :: StopNotebookInstanceResponse
stopNotebookInstanceResponse = StopNotebookInstanceResponse'


instance NFData StopNotebookInstanceResponse where
