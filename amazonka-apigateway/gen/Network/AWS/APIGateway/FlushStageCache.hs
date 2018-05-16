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
-- Module      : Network.AWS.APIGateway.FlushStageCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes a stage's cache.
--
--
module Network.AWS.APIGateway.FlushStageCache
    (
    -- * Creating a Request
      flushStageCache
    , FlushStageCache
    -- * Request Lenses
    , fscRestAPIId
    , fscStageName

    -- * Destructuring the Response
    , flushStageCacheResponse
    , FlushStageCacheResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to flush a stage's cache.
--
--
--
-- /See:/ 'flushStageCache' smart constructor.
data FlushStageCache = FlushStageCache'
  { _fscRestAPIId :: !Text
  , _fscStageName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FlushStageCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fscRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'fscStageName' - [Required] The name of the stage to flush its cache.
flushStageCache
    :: Text -- ^ 'fscRestAPIId'
    -> Text -- ^ 'fscStageName'
    -> FlushStageCache
flushStageCache pRestAPIId_ pStageName_ =
  FlushStageCache' {_fscRestAPIId = pRestAPIId_, _fscStageName = pStageName_}


-- | [Required] The string identifier of the associated 'RestApi' .
fscRestAPIId :: Lens' FlushStageCache Text
fscRestAPIId = lens _fscRestAPIId (\ s a -> s{_fscRestAPIId = a})

-- | [Required] The name of the stage to flush its cache.
fscStageName :: Lens' FlushStageCache Text
fscStageName = lens _fscStageName (\ s a -> s{_fscStageName = a})

instance AWSRequest FlushStageCache where
        type Rs FlushStageCache = FlushStageCacheResponse
        request = delete apiGateway
        response = receiveNull FlushStageCacheResponse'

instance Hashable FlushStageCache where

instance NFData FlushStageCache where

instance ToHeaders FlushStageCache where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath FlushStageCache where
        toPath FlushStageCache'{..}
          = mconcat
              ["/restapis/", toBS _fscRestAPIId, "/stages/",
               toBS _fscStageName, "/cache/data"]

instance ToQuery FlushStageCache where
        toQuery = const mempty

-- | /See:/ 'flushStageCacheResponse' smart constructor.
data FlushStageCacheResponse =
  FlushStageCacheResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FlushStageCacheResponse' with the minimum fields required to make a request.
--
flushStageCacheResponse
    :: FlushStageCacheResponse
flushStageCacheResponse = FlushStageCacheResponse'


instance NFData FlushStageCacheResponse where
