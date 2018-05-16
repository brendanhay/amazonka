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
-- Module      : Network.AWS.APIGateway.FlushStageAuthorizersCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes all authorizer cache entries on a stage.
--
--
module Network.AWS.APIGateway.FlushStageAuthorizersCache
    (
    -- * Creating a Request
      flushStageAuthorizersCache
    , FlushStageAuthorizersCache
    -- * Request Lenses
    , fsacRestAPIId
    , fsacStageName

    -- * Destructuring the Response
    , flushStageAuthorizersCacheResponse
    , FlushStageAuthorizersCacheResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to flush authorizer cache entries on a specified stage.
--
--
--
-- /See:/ 'flushStageAuthorizersCache' smart constructor.
data FlushStageAuthorizersCache = FlushStageAuthorizersCache'
  { _fsacRestAPIId :: !Text
  , _fsacStageName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FlushStageAuthorizersCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsacRestAPIId' - The string identifier of the associated 'RestApi' .
--
-- * 'fsacStageName' - The name of the stage to flush.
flushStageAuthorizersCache
    :: Text -- ^ 'fsacRestAPIId'
    -> Text -- ^ 'fsacStageName'
    -> FlushStageAuthorizersCache
flushStageAuthorizersCache pRestAPIId_ pStageName_ =
  FlushStageAuthorizersCache'
    {_fsacRestAPIId = pRestAPIId_, _fsacStageName = pStageName_}


-- | The string identifier of the associated 'RestApi' .
fsacRestAPIId :: Lens' FlushStageAuthorizersCache Text
fsacRestAPIId = lens _fsacRestAPIId (\ s a -> s{_fsacRestAPIId = a})

-- | The name of the stage to flush.
fsacStageName :: Lens' FlushStageAuthorizersCache Text
fsacStageName = lens _fsacStageName (\ s a -> s{_fsacStageName = a})

instance AWSRequest FlushStageAuthorizersCache where
        type Rs FlushStageAuthorizersCache =
             FlushStageAuthorizersCacheResponse
        request = delete apiGateway
        response
          = receiveNull FlushStageAuthorizersCacheResponse'

instance Hashable FlushStageAuthorizersCache where

instance NFData FlushStageAuthorizersCache where

instance ToHeaders FlushStageAuthorizersCache where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath FlushStageAuthorizersCache where
        toPath FlushStageAuthorizersCache'{..}
          = mconcat
              ["/restapis/", toBS _fsacRestAPIId, "/stages/",
               toBS _fsacStageName, "/cache/authorizers"]

instance ToQuery FlushStageAuthorizersCache where
        toQuery = const mempty

-- | /See:/ 'flushStageAuthorizersCacheResponse' smart constructor.
data FlushStageAuthorizersCacheResponse =
  FlushStageAuthorizersCacheResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FlushStageAuthorizersCacheResponse' with the minimum fields required to make a request.
--
flushStageAuthorizersCacheResponse
    :: FlushStageAuthorizersCacheResponse
flushStageAuthorizersCacheResponse = FlushStageAuthorizersCacheResponse'


instance NFData FlushStageAuthorizersCacheResponse
         where
