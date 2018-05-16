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
-- Module      : Network.AWS.Rekognition.StartStreamProcessor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts processing a stream processor. You create a stream processor by calling . To tell @StartStreamProcessor@ which stream processor to start, use the value of the @Name@ field specified in the call to @CreateStreamProcessor@ .
--
--
module Network.AWS.Rekognition.StartStreamProcessor
    (
    -- * Creating a Request
      startStreamProcessor
    , StartStreamProcessor
    -- * Request Lenses
    , sName

    -- * Destructuring the Response
    , startStreamProcessorResponse
    , StartStreamProcessorResponse
    -- * Response Lenses
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startStreamProcessor' smart constructor.
newtype StartStreamProcessor = StartStreamProcessor'
  { _sName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartStreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sName' - The name of the stream processor to start processing.
startStreamProcessor
    :: Text -- ^ 'sName'
    -> StartStreamProcessor
startStreamProcessor pName_ = StartStreamProcessor' {_sName = pName_}


-- | The name of the stream processor to start processing.
sName :: Lens' StartStreamProcessor Text
sName = lens _sName (\ s a -> s{_sName = a})

instance AWSRequest StartStreamProcessor where
        type Rs StartStreamProcessor =
             StartStreamProcessorResponse
        request = postJSON rekognition
        response
          = receiveEmpty
              (\ s h x ->
                 StartStreamProcessorResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StartStreamProcessor where

instance NFData StartStreamProcessor where

instance ToHeaders StartStreamProcessor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartStreamProcessor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartStreamProcessor where
        toJSON StartStreamProcessor'{..}
          = object (catMaybes [Just ("Name" .= _sName)])

instance ToPath StartStreamProcessor where
        toPath = const "/"

instance ToQuery StartStreamProcessor where
        toQuery = const mempty

-- | /See:/ 'startStreamProcessorResponse' smart constructor.
newtype StartStreamProcessorResponse = StartStreamProcessorResponse'
  { _srsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartStreamProcessorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
startStreamProcessorResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartStreamProcessorResponse
startStreamProcessorResponse pResponseStatus_ =
  StartStreamProcessorResponse' {_srsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StartStreamProcessorResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartStreamProcessorResponse where
