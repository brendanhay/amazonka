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
-- Module      : Network.AWS.SageMaker.DeleteWorkteam
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing work team. This operation can't be undone.
--
--
module Network.AWS.SageMaker.DeleteWorkteam
    (
    -- * Creating a Request
      deleteWorkteam
    , DeleteWorkteam
    -- * Request Lenses
    , dwWorkteamName

    -- * Destructuring the Response
    , deleteWorkteamResponse
    , DeleteWorkteamResponse
    -- * Response Lenses
    , dwrsResponseStatus
    , dwrsSuccess
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteWorkteam' smart constructor.
newtype DeleteWorkteam = DeleteWorkteam'
  { _dwWorkteamName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwWorkteamName' - The name of the work team to delete.
deleteWorkteam
    :: Text -- ^ 'dwWorkteamName'
    -> DeleteWorkteam
deleteWorkteam pWorkteamName_ =
  DeleteWorkteam' {_dwWorkteamName = pWorkteamName_}


-- | The name of the work team to delete.
dwWorkteamName :: Lens' DeleteWorkteam Text
dwWorkteamName = lens _dwWorkteamName (\ s a -> s{_dwWorkteamName = a})

instance AWSRequest DeleteWorkteam where
        type Rs DeleteWorkteam = DeleteWorkteamResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DeleteWorkteamResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Success"))

instance Hashable DeleteWorkteam where

instance NFData DeleteWorkteam where

instance ToHeaders DeleteWorkteam where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteWorkteam" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteWorkteam where
        toJSON DeleteWorkteam'{..}
          = object
              (catMaybes
                 [Just ("WorkteamName" .= _dwWorkteamName)])

instance ToPath DeleteWorkteam where
        toPath = const "/"

instance ToQuery DeleteWorkteam where
        toQuery = const mempty

-- | /See:/ 'deleteWorkteamResponse' smart constructor.
data DeleteWorkteamResponse = DeleteWorkteamResponse'
  { _dwrsResponseStatus :: !Int
  , _dwrsSuccess        :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWorkteamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwrsResponseStatus' - -- | The response status code.
--
-- * 'dwrsSuccess' - Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
deleteWorkteamResponse
    :: Int -- ^ 'dwrsResponseStatus'
    -> Bool -- ^ 'dwrsSuccess'
    -> DeleteWorkteamResponse
deleteWorkteamResponse pResponseStatus_ pSuccess_ =
  DeleteWorkteamResponse'
    {_dwrsResponseStatus = pResponseStatus_, _dwrsSuccess = pSuccess_}


-- | -- | The response status code.
dwrsResponseStatus :: Lens' DeleteWorkteamResponse Int
dwrsResponseStatus = lens _dwrsResponseStatus (\ s a -> s{_dwrsResponseStatus = a})

-- | Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
dwrsSuccess :: Lens' DeleteWorkteamResponse Bool
dwrsSuccess = lens _dwrsSuccess (\ s a -> s{_dwrsSuccess = a})

instance NFData DeleteWorkteamResponse where
