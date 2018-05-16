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
-- Module      : Network.AWS.Glue.StopTrigger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified trigger.
--
--
module Network.AWS.Glue.StopTrigger
    (
    -- * Creating a Request
      stopTrigger
    , StopTrigger
    -- * Request Lenses
    , stName

    -- * Destructuring the Response
    , stopTriggerResponse
    , StopTriggerResponse
    -- * Response Lenses
    , strsName
    , strsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopTrigger' smart constructor.
newtype StopTrigger = StopTrigger'
  { _stName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stName' - The name of the trigger to stop.
stopTrigger
    :: Text -- ^ 'stName'
    -> StopTrigger
stopTrigger pName_ = StopTrigger' {_stName = pName_}


-- | The name of the trigger to stop.
stName :: Lens' StopTrigger Text
stName = lens _stName (\ s a -> s{_stName = a})

instance AWSRequest StopTrigger where
        type Rs StopTrigger = StopTriggerResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 StopTriggerResponse' <$>
                   (x .?> "Name") <*> (pure (fromEnum s)))

instance Hashable StopTrigger where

instance NFData StopTrigger where

instance ToHeaders StopTrigger where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.StopTrigger" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTrigger where
        toJSON StopTrigger'{..}
          = object (catMaybes [Just ("Name" .= _stName)])

instance ToPath StopTrigger where
        toPath = const "/"

instance ToQuery StopTrigger where
        toQuery = const mempty

-- | /See:/ 'stopTriggerResponse' smart constructor.
data StopTriggerResponse = StopTriggerResponse'
  { _strsName           :: !(Maybe Text)
  , _strsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strsName' - The name of the trigger that was stopped.
--
-- * 'strsResponseStatus' - -- | The response status code.
stopTriggerResponse
    :: Int -- ^ 'strsResponseStatus'
    -> StopTriggerResponse
stopTriggerResponse pResponseStatus_ =
  StopTriggerResponse'
    {_strsName = Nothing, _strsResponseStatus = pResponseStatus_}


-- | The name of the trigger that was stopped.
strsName :: Lens' StopTriggerResponse (Maybe Text)
strsName = lens _strsName (\ s a -> s{_strsName = a})

-- | -- | The response status code.
strsResponseStatus :: Lens' StopTriggerResponse Int
strsResponseStatus = lens _strsResponseStatus (\ s a -> s{_strsResponseStatus = a})

instance NFData StopTriggerResponse where
