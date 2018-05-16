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
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker notebook instance. Before you can delete a notebook instance, you must call the @StopNotebookInstance@ API.
--
--
-- /Important:/ When you delete a notebook instance, you lose all of your data. Amazon SageMaker removes the ML compute instance, and deletes the ML storage volume and the network interface associated with the notebook instance.
--
module Network.AWS.SageMaker.DeleteNotebookInstance
    (
    -- * Creating a Request
      deleteNotebookInstance
    , DeleteNotebookInstance
    -- * Request Lenses
    , dNotebookInstanceName

    -- * Destructuring the Response
    , deleteNotebookInstanceResponse
    , DeleteNotebookInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteNotebookInstance' smart constructor.
newtype DeleteNotebookInstance = DeleteNotebookInstance'
  { _dNotebookInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNotebookInstanceName' - The name of the Amazon SageMaker notebook instance to delete.
deleteNotebookInstance
    :: Text -- ^ 'dNotebookInstanceName'
    -> DeleteNotebookInstance
deleteNotebookInstance pNotebookInstanceName_ =
  DeleteNotebookInstance' {_dNotebookInstanceName = pNotebookInstanceName_}


-- | The name of the Amazon SageMaker notebook instance to delete.
dNotebookInstanceName :: Lens' DeleteNotebookInstance Text
dNotebookInstanceName = lens _dNotebookInstanceName (\ s a -> s{_dNotebookInstanceName = a})

instance AWSRequest DeleteNotebookInstance where
        type Rs DeleteNotebookInstance =
             DeleteNotebookInstanceResponse
        request = postJSON sageMaker
        response
          = receiveNull DeleteNotebookInstanceResponse'

instance Hashable DeleteNotebookInstance where

instance NFData DeleteNotebookInstance where

instance ToHeaders DeleteNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteNotebookInstance where
        toJSON DeleteNotebookInstance'{..}
          = object
              (catMaybes
                 [Just
                    ("NotebookInstanceName" .= _dNotebookInstanceName)])

instance ToPath DeleteNotebookInstance where
        toPath = const "/"

instance ToQuery DeleteNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'deleteNotebookInstanceResponse' smart constructor.
data DeleteNotebookInstanceResponse =
  DeleteNotebookInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotebookInstanceResponse' with the minimum fields required to make a request.
--
deleteNotebookInstanceResponse
    :: DeleteNotebookInstanceResponse
deleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'


instance NFData DeleteNotebookInstanceResponse where
