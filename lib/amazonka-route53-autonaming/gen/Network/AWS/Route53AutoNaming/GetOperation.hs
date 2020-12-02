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
-- Module      : Network.AWS.Route53AutoNaming.GetOperation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about any operation that returns an operation ID in the response, such as a @CreateService@ request.
--
--
module Network.AWS.Route53AutoNaming.GetOperation
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
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'getOperation' smart constructor.
newtype GetOperation = GetOperation'
  { _goOperationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goOperationId' - The ID of the operation that you want to get more information about.
getOperation
    :: Text -- ^ 'goOperationId'
    -> GetOperation
getOperation pOperationId_ = GetOperation' {_goOperationId = pOperationId_}


-- | The ID of the operation that you want to get more information about.
goOperationId :: Lens' GetOperation Text
goOperationId = lens _goOperationId (\ s a -> s{_goOperationId = a})

instance AWSRequest GetOperation where
        type Rs GetOperation = GetOperationResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 GetOperationResponse' <$>
                   (x .?> "Operation") <*> (pure (fromEnum s)))

instance Hashable GetOperation where

instance NFData GetOperation where

instance ToHeaders GetOperation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.GetOperation" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOperation where
        toJSON GetOperation'{..}
          = object
              (catMaybes [Just ("OperationId" .= _goOperationId)])

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
-- * 'gorsOperation' - A complex type that contains information about the operation.
--
-- * 'gorsResponseStatus' - -- | The response status code.
getOperationResponse
    :: Int -- ^ 'gorsResponseStatus'
    -> GetOperationResponse
getOperationResponse pResponseStatus_ =
  GetOperationResponse'
    {_gorsOperation = Nothing, _gorsResponseStatus = pResponseStatus_}


-- | A complex type that contains information about the operation.
gorsOperation :: Lens' GetOperationResponse (Maybe Operation)
gorsOperation = lens _gorsOperation (\ s a -> s{_gorsOperation = a})

-- | -- | The response status code.
gorsResponseStatus :: Lens' GetOperationResponse Int
gorsResponseStatus = lens _gorsResponseStatus (\ s a -> s{_gorsResponseStatus = a})

instance NFData GetOperationResponse where
