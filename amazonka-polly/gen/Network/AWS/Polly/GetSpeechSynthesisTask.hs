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
-- Module      : Network.AWS.Polly.GetSpeechSynthesisTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specific SpeechSynthesisTask object based on its TaskID. This object contains information about the given speech synthesis task, including the status of the task, and a link to the S3 bucket containing the output of the task.
--
--
module Network.AWS.Polly.GetSpeechSynthesisTask
    (
    -- * Creating a Request
      getSpeechSynthesisTask
    , GetSpeechSynthesisTask
    -- * Request Lenses
    , gsstTaskId

    -- * Destructuring the Response
    , getSpeechSynthesisTaskResponse
    , GetSpeechSynthesisTaskResponse
    -- * Response Lenses
    , gsstrsSynthesisTask
    , gsstrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSpeechSynthesisTask' smart constructor.
newtype GetSpeechSynthesisTask = GetSpeechSynthesisTask'
  { _gsstTaskId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSpeechSynthesisTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsstTaskId' - The Amazon Polly generated identifier for a speech synthesis task.
getSpeechSynthesisTask
    :: Text -- ^ 'gsstTaskId'
    -> GetSpeechSynthesisTask
getSpeechSynthesisTask pTaskId_ =
  GetSpeechSynthesisTask' {_gsstTaskId = pTaskId_}


-- | The Amazon Polly generated identifier for a speech synthesis task.
gsstTaskId :: Lens' GetSpeechSynthesisTask Text
gsstTaskId = lens _gsstTaskId (\ s a -> s{_gsstTaskId = a})

instance AWSRequest GetSpeechSynthesisTask where
        type Rs GetSpeechSynthesisTask =
             GetSpeechSynthesisTaskResponse
        request = get polly
        response
          = receiveJSON
              (\ s h x ->
                 GetSpeechSynthesisTaskResponse' <$>
                   (x .?> "SynthesisTask") <*> (pure (fromEnum s)))

instance Hashable GetSpeechSynthesisTask where

instance NFData GetSpeechSynthesisTask where

instance ToHeaders GetSpeechSynthesisTask where
        toHeaders = const mempty

instance ToPath GetSpeechSynthesisTask where
        toPath GetSpeechSynthesisTask'{..}
          = mconcat ["/v1/synthesisTasks/", toBS _gsstTaskId]

instance ToQuery GetSpeechSynthesisTask where
        toQuery = const mempty

-- | /See:/ 'getSpeechSynthesisTaskResponse' smart constructor.
data GetSpeechSynthesisTaskResponse = GetSpeechSynthesisTaskResponse'
  { _gsstrsSynthesisTask  :: !(Maybe SynthesisTask)
  , _gsstrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSpeechSynthesisTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsstrsSynthesisTask' - SynthesisTask object that provides information from the requested task, including output format, creation time, task status, and so on.
--
-- * 'gsstrsResponseStatus' - -- | The response status code.
getSpeechSynthesisTaskResponse
    :: Int -- ^ 'gsstrsResponseStatus'
    -> GetSpeechSynthesisTaskResponse
getSpeechSynthesisTaskResponse pResponseStatus_ =
  GetSpeechSynthesisTaskResponse'
    {_gsstrsSynthesisTask = Nothing, _gsstrsResponseStatus = pResponseStatus_}


-- | SynthesisTask object that provides information from the requested task, including output format, creation time, task status, and so on.
gsstrsSynthesisTask :: Lens' GetSpeechSynthesisTaskResponse (Maybe SynthesisTask)
gsstrsSynthesisTask = lens _gsstrsSynthesisTask (\ s a -> s{_gsstrsSynthesisTask = a})

-- | -- | The response status code.
gsstrsResponseStatus :: Lens' GetSpeechSynthesisTaskResponse Int
gsstrsResponseStatus = lens _gsstrsResponseStatus (\ s a -> s{_gsstrsResponseStatus = a})

instance NFData GetSpeechSynthesisTaskResponse where
