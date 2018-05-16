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
-- Module      : Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a lifecycle configuration that you can associate with a notebook instance. A /lifecycle configuration/ is a collection of shell scripts that run when you create or start a notebook instance.
--
--
-- Each lifecycle configuration script has a limit of 16384 characters.
--
-- The value of the @> PATH@ environment variable that is available to both scripts is @/sbin:bin:/usr/sbin:/usr/bin@ .
--
-- View CloudWatch Logs for notebook instance lifecycle configurations in log group @/aws/sagemaker/NotebookInstances@ in log stream @[notebook-instance-name]/[LifecycleConfigHook]@ .
--
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If a script runs for longer than 5 minutes, it fails and the notebook instance is not created or started.
--
-- For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
--
module Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
    (
    -- * Creating a Request
      createNotebookInstanceLifecycleConfig
    , CreateNotebookInstanceLifecycleConfig
    -- * Request Lenses
    , cnilcOnCreate
    , cnilcOnStart
    , cnilcNotebookInstanceLifecycleConfigName

    -- * Destructuring the Response
    , createNotebookInstanceLifecycleConfigResponse
    , CreateNotebookInstanceLifecycleConfigResponse
    -- * Response Lenses
    , cnilcrsNotebookInstanceLifecycleConfigARN
    , cnilcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createNotebookInstanceLifecycleConfig' smart constructor.
data CreateNotebookInstanceLifecycleConfig = CreateNotebookInstanceLifecycleConfig'
  { _cnilcOnCreate :: !(Maybe [NotebookInstanceLifecycleHook])
  , _cnilcOnStart :: !(Maybe [NotebookInstanceLifecycleHook])
  , _cnilcNotebookInstanceLifecycleConfigName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnilcOnCreate' - A shell script that runs only once, when you create a notebook instance.
--
-- * 'cnilcOnStart' - A shell script that runs every time you start a notebook instance, including when you create the notebook instance.
--
-- * 'cnilcNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
createNotebookInstanceLifecycleConfig
    :: Text -- ^ 'cnilcNotebookInstanceLifecycleConfigName'
    -> CreateNotebookInstanceLifecycleConfig
createNotebookInstanceLifecycleConfig pNotebookInstanceLifecycleConfigName_ =
  CreateNotebookInstanceLifecycleConfig'
    { _cnilcOnCreate = Nothing
    , _cnilcOnStart = Nothing
    , _cnilcNotebookInstanceLifecycleConfigName =
        pNotebookInstanceLifecycleConfigName_
    }


-- | A shell script that runs only once, when you create a notebook instance.
cnilcOnCreate :: Lens' CreateNotebookInstanceLifecycleConfig [NotebookInstanceLifecycleHook]
cnilcOnCreate = lens _cnilcOnCreate (\ s a -> s{_cnilcOnCreate = a}) . _Default . _Coerce

-- | A shell script that runs every time you start a notebook instance, including when you create the notebook instance.
cnilcOnStart :: Lens' CreateNotebookInstanceLifecycleConfig [NotebookInstanceLifecycleHook]
cnilcOnStart = lens _cnilcOnStart (\ s a -> s{_cnilcOnStart = a}) . _Default . _Coerce

-- | The name of the lifecycle configuration.
cnilcNotebookInstanceLifecycleConfigName :: Lens' CreateNotebookInstanceLifecycleConfig Text
cnilcNotebookInstanceLifecycleConfigName = lens _cnilcNotebookInstanceLifecycleConfigName (\ s a -> s{_cnilcNotebookInstanceLifecycleConfigName = a})

instance AWSRequest
           CreateNotebookInstanceLifecycleConfig
         where
        type Rs CreateNotebookInstanceLifecycleConfig =
             CreateNotebookInstanceLifecycleConfigResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateNotebookInstanceLifecycleConfigResponse' <$>
                   (x .?> "NotebookInstanceLifecycleConfigArn") <*>
                     (pure (fromEnum s)))

instance Hashable
           CreateNotebookInstanceLifecycleConfig
         where

instance NFData CreateNotebookInstanceLifecycleConfig
         where

instance ToHeaders
           CreateNotebookInstanceLifecycleConfig
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateNotebookInstanceLifecycleConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNotebookInstanceLifecycleConfig
         where
        toJSON CreateNotebookInstanceLifecycleConfig'{..}
          = object
              (catMaybes
                 [("OnCreate" .=) <$> _cnilcOnCreate,
                  ("OnStart" .=) <$> _cnilcOnStart,
                  Just
                    ("NotebookInstanceLifecycleConfigName" .=
                       _cnilcNotebookInstanceLifecycleConfigName)])

instance ToPath CreateNotebookInstanceLifecycleConfig
         where
        toPath = const "/"

instance ToQuery
           CreateNotebookInstanceLifecycleConfig
         where
        toQuery = const mempty

-- | /See:/ 'createNotebookInstanceLifecycleConfigResponse' smart constructor.
data CreateNotebookInstanceLifecycleConfigResponse = CreateNotebookInstanceLifecycleConfigResponse'
  { _cnilcrsNotebookInstanceLifecycleConfigARN :: !(Maybe Text)
  , _cnilcrsResponseStatus                     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnilcrsNotebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- * 'cnilcrsResponseStatus' - -- | The response status code.
createNotebookInstanceLifecycleConfigResponse
    :: Int -- ^ 'cnilcrsResponseStatus'
    -> CreateNotebookInstanceLifecycleConfigResponse
createNotebookInstanceLifecycleConfigResponse pResponseStatus_ =
  CreateNotebookInstanceLifecycleConfigResponse'
    { _cnilcrsNotebookInstanceLifecycleConfigARN = Nothing
    , _cnilcrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
cnilcrsNotebookInstanceLifecycleConfigARN :: Lens' CreateNotebookInstanceLifecycleConfigResponse (Maybe Text)
cnilcrsNotebookInstanceLifecycleConfigARN = lens _cnilcrsNotebookInstanceLifecycleConfigARN (\ s a -> s{_cnilcrsNotebookInstanceLifecycleConfigARN = a})

-- | -- | The response status code.
cnilcrsResponseStatus :: Lens' CreateNotebookInstanceLifecycleConfigResponse Int
cnilcrsResponseStatus = lens _cnilcrsResponseStatus (\ s a -> s{_cnilcrsResponseStatus = a})

instance NFData
           CreateNotebookInstanceLifecycleConfigResponse
         where
