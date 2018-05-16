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
-- Module      : Network.AWS.Rekognition.StopStreamProcessor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running stream processor that was created by .
--
--
module Network.AWS.Rekognition.StopStreamProcessor
    (
    -- * Creating a Request
      stopStreamProcessor
    , StopStreamProcessor
    -- * Request Lenses
    , sspName

    -- * Destructuring the Response
    , stopStreamProcessorResponse
    , StopStreamProcessorResponse
    -- * Response Lenses
    , ssprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopStreamProcessor' smart constructor.
newtype StopStreamProcessor = StopStreamProcessor'
  { _sspName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspName' - The name of a stream processor created by .
stopStreamProcessor
    :: Text -- ^ 'sspName'
    -> StopStreamProcessor
stopStreamProcessor pName_ = StopStreamProcessor' {_sspName = pName_}


-- | The name of a stream processor created by .
sspName :: Lens' StopStreamProcessor Text
sspName = lens _sspName (\ s a -> s{_sspName = a})

instance AWSRequest StopStreamProcessor where
        type Rs StopStreamProcessor =
             StopStreamProcessorResponse
        request = postJSON rekognition
        response
          = receiveEmpty
              (\ s h x ->
                 StopStreamProcessorResponse' <$> (pure (fromEnum s)))

instance Hashable StopStreamProcessor where

instance NFData StopStreamProcessor where

instance ToHeaders StopStreamProcessor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StopStreamProcessor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopStreamProcessor where
        toJSON StopStreamProcessor'{..}
          = object (catMaybes [Just ("Name" .= _sspName)])

instance ToPath StopStreamProcessor where
        toPath = const "/"

instance ToQuery StopStreamProcessor where
        toQuery = const mempty

-- | /See:/ 'stopStreamProcessorResponse' smart constructor.
newtype StopStreamProcessorResponse = StopStreamProcessorResponse'
  { _ssprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStreamProcessorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssprsResponseStatus' - -- | The response status code.
stopStreamProcessorResponse
    :: Int -- ^ 'ssprsResponseStatus'
    -> StopStreamProcessorResponse
stopStreamProcessorResponse pResponseStatus_ =
  StopStreamProcessorResponse' {_ssprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ssprsResponseStatus :: Lens' StopStreamProcessorResponse Int
ssprsResponseStatus = lens _ssprsResponseStatus (\ s a -> s{_ssprsResponseStatus = a})

instance NFData StopStreamProcessorResponse where
