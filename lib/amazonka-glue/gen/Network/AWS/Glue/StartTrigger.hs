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
-- Module      : Network.AWS.Glue.StartTrigger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing trigger. See <http://docs.aws.amazon.com/glue/latest/dg/trigger-job.html Triggering Jobs> for information about how different types of trigger are started.
--
--
module Network.AWS.Glue.StartTrigger
    (
    -- * Creating a Request
      startTrigger
    , StartTrigger
    -- * Request Lenses
    , staName

    -- * Destructuring the Response
    , startTriggerResponse
    , StartTriggerResponse
    -- * Response Lenses
    , starsName
    , starsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startTrigger' smart constructor.
newtype StartTrigger = StartTrigger'
  { _staName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staName' - The name of the trigger to start.
startTrigger
    :: Text -- ^ 'staName'
    -> StartTrigger
startTrigger pName_ = StartTrigger' {_staName = pName_}


-- | The name of the trigger to start.
staName :: Lens' StartTrigger Text
staName = lens _staName (\ s a -> s{_staName = a})

instance AWSRequest StartTrigger where
        type Rs StartTrigger = StartTriggerResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 StartTriggerResponse' <$>
                   (x .?> "Name") <*> (pure (fromEnum s)))

instance Hashable StartTrigger where

instance NFData StartTrigger where

instance ToHeaders StartTrigger where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.StartTrigger" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartTrigger where
        toJSON StartTrigger'{..}
          = object (catMaybes [Just ("Name" .= _staName)])

instance ToPath StartTrigger where
        toPath = const "/"

instance ToQuery StartTrigger where
        toQuery = const mempty

-- | /See:/ 'startTriggerResponse' smart constructor.
data StartTriggerResponse = StartTriggerResponse'
  { _starsName           :: !(Maybe Text)
  , _starsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'starsName' - The name of the trigger that was started.
--
-- * 'starsResponseStatus' - -- | The response status code.
startTriggerResponse
    :: Int -- ^ 'starsResponseStatus'
    -> StartTriggerResponse
startTriggerResponse pResponseStatus_ =
  StartTriggerResponse'
    {_starsName = Nothing, _starsResponseStatus = pResponseStatus_}


-- | The name of the trigger that was started.
starsName :: Lens' StartTriggerResponse (Maybe Text)
starsName = lens _starsName (\ s a -> s{_starsName = a})

-- | -- | The response status code.
starsResponseStatus :: Lens' StartTriggerResponse Int
starsResponseStatus = lens _starsResponseStatus (\ s a -> s{_starsResponseStatus = a})

instance NFData StartTriggerResponse where
