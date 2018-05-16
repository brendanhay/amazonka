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
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance lifecycle configuration created with the API.
--
--
module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
    (
    -- * Creating a Request
      updateNotebookInstanceLifecycleConfig
    , UpdateNotebookInstanceLifecycleConfig
    -- * Request Lenses
    , unilcOnCreate
    , unilcOnStart
    , unilcNotebookInstanceLifecycleConfigName

    -- * Destructuring the Response
    , updateNotebookInstanceLifecycleConfigResponse
    , UpdateNotebookInstanceLifecycleConfigResponse
    -- * Response Lenses
    , unilcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'updateNotebookInstanceLifecycleConfig' smart constructor.
data UpdateNotebookInstanceLifecycleConfig = UpdateNotebookInstanceLifecycleConfig'
  { _unilcOnCreate :: !(Maybe [NotebookInstanceLifecycleHook])
  , _unilcOnStart :: !(Maybe [NotebookInstanceLifecycleHook])
  , _unilcNotebookInstanceLifecycleConfigName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unilcOnCreate' - The shell script that runs only once, when you create a notebook instance
--
-- * 'unilcOnStart' - The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
--
-- * 'unilcNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
updateNotebookInstanceLifecycleConfig
    :: Text -- ^ 'unilcNotebookInstanceLifecycleConfigName'
    -> UpdateNotebookInstanceLifecycleConfig
updateNotebookInstanceLifecycleConfig pNotebookInstanceLifecycleConfigName_ =
  UpdateNotebookInstanceLifecycleConfig'
    { _unilcOnCreate = Nothing
    , _unilcOnStart = Nothing
    , _unilcNotebookInstanceLifecycleConfigName =
        pNotebookInstanceLifecycleConfigName_
    }


-- | The shell script that runs only once, when you create a notebook instance
unilcOnCreate :: Lens' UpdateNotebookInstanceLifecycleConfig [NotebookInstanceLifecycleHook]
unilcOnCreate = lens _unilcOnCreate (\ s a -> s{_unilcOnCreate = a}) . _Default . _Coerce

-- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
unilcOnStart :: Lens' UpdateNotebookInstanceLifecycleConfig [NotebookInstanceLifecycleHook]
unilcOnStart = lens _unilcOnStart (\ s a -> s{_unilcOnStart = a}) . _Default . _Coerce

-- | The name of the lifecycle configuration.
unilcNotebookInstanceLifecycleConfigName :: Lens' UpdateNotebookInstanceLifecycleConfig Text
unilcNotebookInstanceLifecycleConfigName = lens _unilcNotebookInstanceLifecycleConfigName (\ s a -> s{_unilcNotebookInstanceLifecycleConfigName = a})

instance AWSRequest
           UpdateNotebookInstanceLifecycleConfig
         where
        type Rs UpdateNotebookInstanceLifecycleConfig =
             UpdateNotebookInstanceLifecycleConfigResponse
        request = postJSON sageMaker
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateNotebookInstanceLifecycleConfigResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           UpdateNotebookInstanceLifecycleConfig
         where

instance NFData UpdateNotebookInstanceLifecycleConfig
         where

instance ToHeaders
           UpdateNotebookInstanceLifecycleConfig
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.UpdateNotebookInstanceLifecycleConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNotebookInstanceLifecycleConfig
         where
        toJSON UpdateNotebookInstanceLifecycleConfig'{..}
          = object
              (catMaybes
                 [("OnCreate" .=) <$> _unilcOnCreate,
                  ("OnStart" .=) <$> _unilcOnStart,
                  Just
                    ("NotebookInstanceLifecycleConfigName" .=
                       _unilcNotebookInstanceLifecycleConfigName)])

instance ToPath UpdateNotebookInstanceLifecycleConfig
         where
        toPath = const "/"

instance ToQuery
           UpdateNotebookInstanceLifecycleConfig
         where
        toQuery = const mempty

-- | /See:/ 'updateNotebookInstanceLifecycleConfigResponse' smart constructor.
newtype UpdateNotebookInstanceLifecycleConfigResponse = UpdateNotebookInstanceLifecycleConfigResponse'
  { _unilcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unilcrsResponseStatus' - -- | The response status code.
updateNotebookInstanceLifecycleConfigResponse
    :: Int -- ^ 'unilcrsResponseStatus'
    -> UpdateNotebookInstanceLifecycleConfigResponse
updateNotebookInstanceLifecycleConfigResponse pResponseStatus_ =
  UpdateNotebookInstanceLifecycleConfigResponse'
    {_unilcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
unilcrsResponseStatus :: Lens' UpdateNotebookInstanceLifecycleConfigResponse Int
unilcrsResponseStatus = lens _unilcrsResponseStatus (\ s a -> s{_unilcrsResponseStatus = a})

instance NFData
           UpdateNotebookInstanceLifecycleConfigResponse
         where
