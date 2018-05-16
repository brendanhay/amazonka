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
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notebook instance lifecycle configuration.
--
--
module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
    (
    -- * Creating a Request
      deleteNotebookInstanceLifecycleConfig
    , DeleteNotebookInstanceLifecycleConfig
    -- * Request Lenses
    , dnilcNotebookInstanceLifecycleConfigName

    -- * Destructuring the Response
    , deleteNotebookInstanceLifecycleConfigResponse
    , DeleteNotebookInstanceLifecycleConfigResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteNotebookInstanceLifecycleConfig' smart constructor.
newtype DeleteNotebookInstanceLifecycleConfig = DeleteNotebookInstanceLifecycleConfig'
  { _dnilcNotebookInstanceLifecycleConfigName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnilcNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration to delete.
deleteNotebookInstanceLifecycleConfig
    :: Text -- ^ 'dnilcNotebookInstanceLifecycleConfigName'
    -> DeleteNotebookInstanceLifecycleConfig
deleteNotebookInstanceLifecycleConfig pNotebookInstanceLifecycleConfigName_ =
  DeleteNotebookInstanceLifecycleConfig'
    { _dnilcNotebookInstanceLifecycleConfigName =
        pNotebookInstanceLifecycleConfigName_
    }


-- | The name of the lifecycle configuration to delete.
dnilcNotebookInstanceLifecycleConfigName :: Lens' DeleteNotebookInstanceLifecycleConfig Text
dnilcNotebookInstanceLifecycleConfigName = lens _dnilcNotebookInstanceLifecycleConfigName (\ s a -> s{_dnilcNotebookInstanceLifecycleConfigName = a})

instance AWSRequest
           DeleteNotebookInstanceLifecycleConfig
         where
        type Rs DeleteNotebookInstanceLifecycleConfig =
             DeleteNotebookInstanceLifecycleConfigResponse
        request = postJSON sageMaker
        response
          = receiveNull
              DeleteNotebookInstanceLifecycleConfigResponse'

instance Hashable
           DeleteNotebookInstanceLifecycleConfig
         where

instance NFData DeleteNotebookInstanceLifecycleConfig
         where

instance ToHeaders
           DeleteNotebookInstanceLifecycleConfig
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteNotebookInstanceLifecycleConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteNotebookInstanceLifecycleConfig
         where
        toJSON DeleteNotebookInstanceLifecycleConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("NotebookInstanceLifecycleConfigName" .=
                       _dnilcNotebookInstanceLifecycleConfigName)])

instance ToPath DeleteNotebookInstanceLifecycleConfig
         where
        toPath = const "/"

instance ToQuery
           DeleteNotebookInstanceLifecycleConfig
         where
        toQuery = const mempty

-- | /See:/ 'deleteNotebookInstanceLifecycleConfigResponse' smart constructor.
data DeleteNotebookInstanceLifecycleConfigResponse =
  DeleteNotebookInstanceLifecycleConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
deleteNotebookInstanceLifecycleConfigResponse
    :: DeleteNotebookInstanceLifecycleConfigResponse
deleteNotebookInstanceLifecycleConfigResponse =
  DeleteNotebookInstanceLifecycleConfigResponse'


instance NFData
           DeleteNotebookInstanceLifecycleConfigResponse
         where
