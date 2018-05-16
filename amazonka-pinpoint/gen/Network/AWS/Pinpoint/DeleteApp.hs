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
-- Module      : Network.AWS.Pinpoint.DeleteApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an app.
module Network.AWS.Pinpoint.DeleteApp
    (
    -- * Creating a Request
      deleteApp
    , DeleteApp
    -- * Request Lenses
    , daApplicationId

    -- * Destructuring the Response
    , deleteAppResponse
    , DeleteAppResponse
    -- * Response Lenses
    , darsResponseStatus
    , darsApplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApp' smart constructor.
newtype DeleteApp = DeleteApp'
  { _daApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daApplicationId' - Undocumented member.
deleteApp
    :: Text -- ^ 'daApplicationId'
    -> DeleteApp
deleteApp pApplicationId_ = DeleteApp' {_daApplicationId = pApplicationId_}


-- | Undocumented member.
daApplicationId :: Lens' DeleteApp Text
daApplicationId = lens _daApplicationId (\ s a -> s{_daApplicationId = a})

instance AWSRequest DeleteApp where
        type Rs DeleteApp = DeleteAppResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAppResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteApp where

instance NFData DeleteApp where

instance ToHeaders DeleteApp where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteApp where
        toPath DeleteApp'{..}
          = mconcat ["/v1/apps/", toBS _daApplicationId]

instance ToQuery DeleteApp where
        toQuery = const mempty

-- | /See:/ 'deleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  { _darsResponseStatus      :: !Int
  , _darsApplicationResponse :: !ApplicationResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
--
-- * 'darsApplicationResponse' - Undocumented member.
deleteAppResponse
    :: Int -- ^ 'darsResponseStatus'
    -> ApplicationResponse -- ^ 'darsApplicationResponse'
    -> DeleteAppResponse
deleteAppResponse pResponseStatus_ pApplicationResponse_ =
  DeleteAppResponse'
    { _darsResponseStatus = pResponseStatus_
    , _darsApplicationResponse = pApplicationResponse_
    }


-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAppResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

-- | Undocumented member.
darsApplicationResponse :: Lens' DeleteAppResponse ApplicationResponse
darsApplicationResponse = lens _darsApplicationResponse (\ s a -> s{_darsApplicationResponse = a})

instance NFData DeleteAppResponse where
