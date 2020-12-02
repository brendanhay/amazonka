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
-- Module      : Network.AWS.Glue.GetTrigger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
--
--
module Network.AWS.Glue.GetTrigger
    (
    -- * Creating a Request
      getTrigger
    , GetTrigger
    -- * Request Lenses
    , gtName

    -- * Destructuring the Response
    , getTriggerResponse
    , GetTriggerResponse
    -- * Response Lenses
    , gtrsTrigger
    , gtrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTrigger' smart constructor.
newtype GetTrigger = GetTrigger'
  { _gtName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtName' - The name of the trigger to retrieve.
getTrigger
    :: Text -- ^ 'gtName'
    -> GetTrigger
getTrigger pName_ = GetTrigger' {_gtName = pName_}


-- | The name of the trigger to retrieve.
gtName :: Lens' GetTrigger Text
gtName = lens _gtName (\ s a -> s{_gtName = a})

instance AWSRequest GetTrigger where
        type Rs GetTrigger = GetTriggerResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetTriggerResponse' <$>
                   (x .?> "Trigger") <*> (pure (fromEnum s)))

instance Hashable GetTrigger where

instance NFData GetTrigger where

instance ToHeaders GetTrigger where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetTrigger" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTrigger where
        toJSON GetTrigger'{..}
          = object (catMaybes [Just ("Name" .= _gtName)])

instance ToPath GetTrigger where
        toPath = const "/"

instance ToQuery GetTrigger where
        toQuery = const mempty

-- | /See:/ 'getTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
  { _gtrsTrigger        :: !(Maybe Trigger)
  , _gtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTrigger' - The requested trigger definition.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTriggerResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTriggerResponse
getTriggerResponse pResponseStatus_ =
  GetTriggerResponse'
    {_gtrsTrigger = Nothing, _gtrsResponseStatus = pResponseStatus_}


-- | The requested trigger definition.
gtrsTrigger :: Lens' GetTriggerResponse (Maybe Trigger)
gtrsTrigger = lens _gtrsTrigger (\ s a -> s{_gtrsTrigger = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTriggerResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTriggerResponse where
