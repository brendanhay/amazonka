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
-- Module      : Network.AWS.SageMaker.StartNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an ML compute instance with the latest version of the libraries and attaches your ML storage volume. After configuring the notebook instance, Amazon SageMaker sets the notebook instance status to @InService@ . A notebook instance's status must be @InService@ before you can connect to your Jupyter notebook.
--
--
module Network.AWS.SageMaker.StartNotebookInstance
    (
    -- * Creating a Request
      startNotebookInstance
    , StartNotebookInstance
    -- * Request Lenses
    , sNotebookInstanceName

    -- * Destructuring the Response
    , startNotebookInstanceResponse
    , StartNotebookInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'startNotebookInstance' smart constructor.
newtype StartNotebookInstance = StartNotebookInstance'
  { _sNotebookInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNotebookInstanceName' - The name of the notebook instance to start.
startNotebookInstance
    :: Text -- ^ 'sNotebookInstanceName'
    -> StartNotebookInstance
startNotebookInstance pNotebookInstanceName_ =
  StartNotebookInstance' {_sNotebookInstanceName = pNotebookInstanceName_}


-- | The name of the notebook instance to start.
sNotebookInstanceName :: Lens' StartNotebookInstance Text
sNotebookInstanceName = lens _sNotebookInstanceName (\ s a -> s{_sNotebookInstanceName = a})

instance AWSRequest StartNotebookInstance where
        type Rs StartNotebookInstance =
             StartNotebookInstanceResponse
        request = postJSON sageMaker
        response = receiveNull StartNotebookInstanceResponse'

instance Hashable StartNotebookInstance where

instance NFData StartNotebookInstance where

instance ToHeaders StartNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.StartNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartNotebookInstance where
        toJSON StartNotebookInstance'{..}
          = object
              (catMaybes
                 [Just
                    ("NotebookInstanceName" .= _sNotebookInstanceName)])

instance ToPath StartNotebookInstance where
        toPath = const "/"

instance ToQuery StartNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'startNotebookInstanceResponse' smart constructor.
data StartNotebookInstanceResponse =
  StartNotebookInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNotebookInstanceResponse' with the minimum fields required to make a request.
--
startNotebookInstanceResponse
    :: StartNotebookInstanceResponse
startNotebookInstanceResponse = StartNotebookInstanceResponse'


instance NFData StartNotebookInstanceResponse where
