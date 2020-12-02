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
-- Module      : Network.AWS.Glue.UpdateTrigger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a trigger definition.
--
--
module Network.AWS.Glue.UpdateTrigger
    (
    -- * Creating a Request
      updateTrigger
    , UpdateTrigger
    -- * Request Lenses
    , utName
    , utTriggerUpdate

    -- * Destructuring the Response
    , updateTriggerResponse
    , UpdateTriggerResponse
    -- * Response Lenses
    , updrsTrigger
    , updrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTrigger' smart constructor.
data UpdateTrigger = UpdateTrigger'
  { _utName          :: !Text
  , _utTriggerUpdate :: !TriggerUpdate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utName' - The name of the trigger to update.
--
-- * 'utTriggerUpdate' - The new values with which to update the trigger.
updateTrigger
    :: Text -- ^ 'utName'
    -> TriggerUpdate -- ^ 'utTriggerUpdate'
    -> UpdateTrigger
updateTrigger pName_ pTriggerUpdate_ =
  UpdateTrigger' {_utName = pName_, _utTriggerUpdate = pTriggerUpdate_}


-- | The name of the trigger to update.
utName :: Lens' UpdateTrigger Text
utName = lens _utName (\ s a -> s{_utName = a})

-- | The new values with which to update the trigger.
utTriggerUpdate :: Lens' UpdateTrigger TriggerUpdate
utTriggerUpdate = lens _utTriggerUpdate (\ s a -> s{_utTriggerUpdate = a})

instance AWSRequest UpdateTrigger where
        type Rs UpdateTrigger = UpdateTriggerResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTriggerResponse' <$>
                   (x .?> "Trigger") <*> (pure (fromEnum s)))

instance Hashable UpdateTrigger where

instance NFData UpdateTrigger where

instance ToHeaders UpdateTrigger where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateTrigger" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTrigger where
        toJSON UpdateTrigger'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _utName),
                  Just ("TriggerUpdate" .= _utTriggerUpdate)])

instance ToPath UpdateTrigger where
        toPath = const "/"

instance ToQuery UpdateTrigger where
        toQuery = const mempty

-- | /See:/ 'updateTriggerResponse' smart constructor.
data UpdateTriggerResponse = UpdateTriggerResponse'
  { _updrsTrigger        :: !(Maybe Trigger)
  , _updrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updrsTrigger' - The resulting trigger definition.
--
-- * 'updrsResponseStatus' - -- | The response status code.
updateTriggerResponse
    :: Int -- ^ 'updrsResponseStatus'
    -> UpdateTriggerResponse
updateTriggerResponse pResponseStatus_ =
  UpdateTriggerResponse'
    {_updrsTrigger = Nothing, _updrsResponseStatus = pResponseStatus_}


-- | The resulting trigger definition.
updrsTrigger :: Lens' UpdateTriggerResponse (Maybe Trigger)
updrsTrigger = lens _updrsTrigger (\ s a -> s{_updrsTrigger = a})

-- | -- | The response status code.
updrsResponseStatus :: Lens' UpdateTriggerResponse Int
updrsResponseStatus = lens _updrsResponseStatus (\ s a -> s{_updrsResponseStatus = a})

instance NFData UpdateTriggerResponse where
