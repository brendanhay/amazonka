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
-- Module      : Network.AWS.Lightsail.GetOperation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.
--
--
module Network.AWS.Lightsail.GetOperation
    (
    -- * Creating a Request
      getOperation
    , GetOperation
    -- * Request Lenses
    , goOperationId

    -- * Destructuring the Response
    , getOperationResponse
    , GetOperationResponse
    -- * Response Lenses
    , gorsOperation
    , gorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOperation' smart constructor.
newtype GetOperation = GetOperation'
  { _goOperationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goOperationId' - A GUID used to identify the operation.
getOperation
    :: Text -- ^ 'goOperationId'
    -> GetOperation
getOperation pOperationId_ = GetOperation' {_goOperationId = pOperationId_}


-- | A GUID used to identify the operation.
goOperationId :: Lens' GetOperation Text
goOperationId = lens _goOperationId (\ s a -> s{_goOperationId = a})

instance AWSRequest GetOperation where
        type Rs GetOperation = GetOperationResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetOperationResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable GetOperation where

instance NFData GetOperation where

instance ToHeaders GetOperation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetOperation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOperation where
        toJSON GetOperation'{..}
          = object
              (catMaybes [Just ("operationId" .= _goOperationId)])

instance ToPath GetOperation where
        toPath = const "/"

instance ToQuery GetOperation where
        toQuery = const mempty

-- | /See:/ 'getOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { _gorsOperation      :: !(Maybe Operation)
  , _gorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorsOperation' - An array of key-value pairs containing information about the results of your get operation request.
--
-- * 'gorsResponseStatus' - -- | The response status code.
getOperationResponse
    :: Int -- ^ 'gorsResponseStatus'
    -> GetOperationResponse
getOperationResponse pResponseStatus_ =
  GetOperationResponse'
    {_gorsOperation = Nothing, _gorsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your get operation request.
gorsOperation :: Lens' GetOperationResponse (Maybe Operation)
gorsOperation = lens _gorsOperation (\ s a -> s{_gorsOperation = a})

-- | -- | The response status code.
gorsResponseStatus :: Lens' GetOperationResponse Int
gorsResponseStatus = lens _gorsResponseStatus (\ s a -> s{_gorsResponseStatus = a})

instance NFData GetOperationResponse where
