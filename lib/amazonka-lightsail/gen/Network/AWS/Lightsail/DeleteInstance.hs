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
-- Module      : Network.AWS.Lightsail.DeleteInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific Amazon Lightsail virtual private server, or /instance/ .
--
--
module Network.AWS.Lightsail.DeleteInstance
    (
    -- * Creating a Request
      deleteInstance
    , DeleteInstance
    -- * Request Lenses
    , diInstanceName

    -- * Destructuring the Response
    , deleteInstanceResponse
    , DeleteInstanceResponse
    -- * Response Lenses
    , dirsOperations
    , dirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInstance' smart constructor.
newtype DeleteInstance = DeleteInstance'
  { _diInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diInstanceName' - The name of the instance to delete.
deleteInstance
    :: Text -- ^ 'diInstanceName'
    -> DeleteInstance
deleteInstance pInstanceName_ =
  DeleteInstance' {_diInstanceName = pInstanceName_}


-- | The name of the instance to delete.
diInstanceName :: Lens' DeleteInstance Text
diInstanceName = lens _diInstanceName (\ s a -> s{_diInstanceName = a})

instance AWSRequest DeleteInstance where
        type Rs DeleteInstance = DeleteInstanceResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteInstanceResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteInstance where

instance NFData DeleteInstance where

instance ToHeaders DeleteInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteInstance where
        toJSON DeleteInstance'{..}
          = object
              (catMaybes
                 [Just ("instanceName" .= _diInstanceName)])

instance ToPath DeleteInstance where
        toPath = const "/"

instance ToQuery DeleteInstance where
        toQuery = const mempty

-- | /See:/ 'deleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  { _dirsOperations     :: !(Maybe [Operation])
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsOperations' - An array of key-value pairs containing information about the results of your delete instance request.
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteInstanceResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeleteInstanceResponse
deleteInstanceResponse pResponseStatus_ =
  DeleteInstanceResponse'
    {_dirsOperations = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your delete instance request.
dirsOperations :: Lens' DeleteInstanceResponse [Operation]
dirsOperations = lens _dirsOperations (\ s a -> s{_dirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteInstanceResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DeleteInstanceResponse where
