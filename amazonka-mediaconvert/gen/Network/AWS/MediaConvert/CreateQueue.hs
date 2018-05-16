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
-- Module      : Network.AWS.MediaConvert.CreateQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding queue. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateQueue
    (
    -- * Creating a Request
      createQueue
    , CreateQueue
    -- * Request Lenses
    , cqName
    , cqDescription

    -- * Destructuring the Response
    , createQueueResponse
    , CreateQueueResponse
    -- * Response Lenses
    , cqrsQueue
    , cqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createQueue' smart constructor.
data CreateQueue = CreateQueue'
  { _cqName        :: !(Maybe Text)
  , _cqDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqName' - The name of the queue you are creating.
--
-- * 'cqDescription' - Optional. A description of the queue you are creating.
createQueue
    :: CreateQueue
createQueue = CreateQueue' {_cqName = Nothing, _cqDescription = Nothing}


-- | The name of the queue you are creating.
cqName :: Lens' CreateQueue (Maybe Text)
cqName = lens _cqName (\ s a -> s{_cqName = a})

-- | Optional. A description of the queue you are creating.
cqDescription :: Lens' CreateQueue (Maybe Text)
cqDescription = lens _cqDescription (\ s a -> s{_cqDescription = a})

instance AWSRequest CreateQueue where
        type Rs CreateQueue = CreateQueueResponse
        request = postJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 CreateQueueResponse' <$>
                   (x .?> "queue") <*> (pure (fromEnum s)))

instance Hashable CreateQueue where

instance NFData CreateQueue where

instance ToHeaders CreateQueue where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateQueue where
        toJSON CreateQueue'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _cqName,
                  ("description" .=) <$> _cqDescription])

instance ToPath CreateQueue where
        toPath = const "/2017-08-29/queues"

instance ToQuery CreateQueue where
        toQuery = const mempty

-- | /See:/ 'createQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { _cqrsQueue          :: !(Maybe Queue)
  , _cqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqrsQueue' - Undocumented member.
--
-- * 'cqrsResponseStatus' - -- | The response status code.
createQueueResponse
    :: Int -- ^ 'cqrsResponseStatus'
    -> CreateQueueResponse
createQueueResponse pResponseStatus_ =
  CreateQueueResponse'
    {_cqrsQueue = Nothing, _cqrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cqrsQueue :: Lens' CreateQueueResponse (Maybe Queue)
cqrsQueue = lens _cqrsQueue (\ s a -> s{_cqrsQueue = a})

-- | -- | The response status code.
cqrsResponseStatus :: Lens' CreateQueueResponse Int
cqrsResponseStatus = lens _cqrsResponseStatus (\ s a -> s{_cqrsResponseStatus = a})

instance NFData CreateQueueResponse where
