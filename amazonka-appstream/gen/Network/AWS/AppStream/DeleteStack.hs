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
-- Module      : Network.AWS.AppStream.DeleteStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified stack. After this operation completes, the environment can no longer be activated and any reservations made for the stack are released.
--
--
module Network.AWS.AppStream.DeleteStack
    (
    -- * Creating a Request
      deleteStack
    , DeleteStack
    -- * Request Lenses
    , dsName

    -- * Destructuring the Response
    , deleteStackResponse
    , DeleteStackResponse
    -- * Response Lenses
    , dsrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStack' smart constructor.
newtype DeleteStack = DeleteStack'
  { _dsName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsName' - The name of the stack.
deleteStack
    :: Text -- ^ 'dsName'
    -> DeleteStack
deleteStack pName_ = DeleteStack' {_dsName = pName_}


-- | The name of the stack.
dsName :: Lens' DeleteStack Text
dsName = lens _dsName (\ s a -> s{_dsName = a})

instance AWSRequest DeleteStack where
        type Rs DeleteStack = DeleteStackResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteStackResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteStack where

instance NFData DeleteStack where

instance ToHeaders DeleteStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DeleteStack" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteStack where
        toJSON DeleteStack'{..}
          = object (catMaybes [Just ("Name" .= _dsName)])

instance ToPath DeleteStack where
        toPath = const "/"

instance ToQuery DeleteStack where
        toQuery = const mempty

-- | /See:/ 'deleteStackResponse' smart constructor.
newtype DeleteStackResponse = DeleteStackResponse'
  { _dsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteStackResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteStackResponse
deleteStackResponse pResponseStatus_ =
  DeleteStackResponse' {_dsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteStackResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteStackResponse where
