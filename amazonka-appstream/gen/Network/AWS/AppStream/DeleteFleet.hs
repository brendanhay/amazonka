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
-- Module      : Network.AWS.AppStream.DeleteFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified fleet.
--
--
module Network.AWS.AppStream.DeleteFleet
    (
    -- * Creating a Request
      deleteFleet
    , DeleteFleet
    -- * Request Lenses
    , dfName

    -- * Destructuring the Response
    , deleteFleetResponse
    , DeleteFleetResponse
    -- * Response Lenses
    , dfrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFleet' smart constructor.
newtype DeleteFleet = DeleteFleet'
  { _dfName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfName' - The name of the fleet.
deleteFleet
    :: Text -- ^ 'dfName'
    -> DeleteFleet
deleteFleet pName_ = DeleteFleet' {_dfName = pName_}


-- | The name of the fleet.
dfName :: Lens' DeleteFleet Text
dfName = lens _dfName (\ s a -> s{_dfName = a})

instance AWSRequest DeleteFleet where
        type Rs DeleteFleet = DeleteFleetResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteFleetResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteFleet where

instance NFData DeleteFleet where

instance ToHeaders DeleteFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DeleteFleet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteFleet where
        toJSON DeleteFleet'{..}
          = object (catMaybes [Just ("Name" .= _dfName)])

instance ToPath DeleteFleet where
        toPath = const "/"

instance ToQuery DeleteFleet where
        toQuery = const mempty

-- | /See:/ 'deleteFleetResponse' smart constructor.
newtype DeleteFleetResponse = DeleteFleetResponse'
  { _dfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsResponseStatus' - -- | The response status code.
deleteFleetResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DeleteFleetResponse
deleteFleetResponse pResponseStatus_ =
  DeleteFleetResponse' {_dfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dfrsResponseStatus :: Lens' DeleteFleetResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

instance NFData DeleteFleetResponse where
