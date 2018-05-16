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
-- Module      : Network.AWS.SSM.DeleteActivation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activation. You are not required to delete an activation. If you delete an activation, you can no longer use it to register additional managed instances. Deleting an activation does not de-register managed instances. You must manually de-register managed instances.
--
--
module Network.AWS.SSM.DeleteActivation
    (
    -- * Creating a Request
      deleteActivation
    , DeleteActivation
    -- * Request Lenses
    , daActivationId

    -- * Destructuring the Response
    , deleteActivationResponse
    , DeleteActivationResponse
    -- * Response Lenses
    , delersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteActivation' smart constructor.
newtype DeleteActivation = DeleteActivation'
  { _daActivationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteActivation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daActivationId' - The ID of the activation that you want to delete.
deleteActivation
    :: Text -- ^ 'daActivationId'
    -> DeleteActivation
deleteActivation pActivationId_ =
  DeleteActivation' {_daActivationId = pActivationId_}


-- | The ID of the activation that you want to delete.
daActivationId :: Lens' DeleteActivation Text
daActivationId = lens _daActivationId (\ s a -> s{_daActivationId = a})

instance AWSRequest DeleteActivation where
        type Rs DeleteActivation = DeleteActivationResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteActivationResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteActivation where

instance NFData DeleteActivation where

instance ToHeaders DeleteActivation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteActivation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteActivation where
        toJSON DeleteActivation'{..}
          = object
              (catMaybes
                 [Just ("ActivationId" .= _daActivationId)])

instance ToPath DeleteActivation where
        toPath = const "/"

instance ToQuery DeleteActivation where
        toQuery = const mempty

-- | /See:/ 'deleteActivationResponse' smart constructor.
newtype DeleteActivationResponse = DeleteActivationResponse'
  { _delersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteActivationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delersResponseStatus' - -- | The response status code.
deleteActivationResponse
    :: Int -- ^ 'delersResponseStatus'
    -> DeleteActivationResponse
deleteActivationResponse pResponseStatus_ =
  DeleteActivationResponse' {_delersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delersResponseStatus :: Lens' DeleteActivationResponse Int
delersResponseStatus = lens _delersResponseStatus (\ s a -> s{_delersResponseStatus = a})

instance NFData DeleteActivationResponse where
