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
-- Module      : Network.AWS.IoT.UpdateThing
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a thing.
module Network.AWS.IoT.UpdateThing
    (
    -- * Creating a Request
      updateThing
    , UpdateThing
    -- * Request Lenses
    , utThingName
    , utAttributePayload

    -- * Destructuring the Response
    , updateThingResponse
    , UpdateThingResponse
    -- * Response Lenses
    , utrsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the UpdateThing operation.
--
-- /See:/ 'updateThing' smart constructor.
data UpdateThing = UpdateThing'
    { _utThingName        :: !Text
    , _utAttributePayload :: !AttributePayload
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utThingName'
--
-- * 'utAttributePayload'
updateThing
    :: Text -- ^ 'utThingName'
    -> AttributePayload -- ^ 'utAttributePayload'
    -> UpdateThing
updateThing pThingName_ pAttributePayload_ =
    UpdateThing'
    { _utThingName = pThingName_
    , _utAttributePayload = pAttributePayload_
    }

-- | The thing name.
utThingName :: Lens' UpdateThing Text
utThingName = lens _utThingName (\ s a -> s{_utThingName = a});

-- | The attribute payload, a JSON string containing up to three key-value
-- pairs (for example,
-- {\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}).
utAttributePayload :: Lens' UpdateThing AttributePayload
utAttributePayload = lens _utAttributePayload (\ s a -> s{_utAttributePayload = a});

instance AWSRequest UpdateThing where
        type Rs UpdateThing = UpdateThingResponse
        request = patchJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateThingResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateThing

instance NFData UpdateThing

instance ToHeaders UpdateThing where
        toHeaders = const mempty

instance ToJSON UpdateThing where
        toJSON UpdateThing'{..}
          = object
              (catMaybes
                 [Just ("attributePayload" .= _utAttributePayload)])

instance ToPath UpdateThing where
        toPath UpdateThing'{..}
          = mconcat ["/things/", toBS _utThingName]

instance ToQuery UpdateThing where
        toQuery = const mempty

-- | The output from the UpdateThing operation.
--
-- /See:/ 'updateThingResponse' smart constructor.
newtype UpdateThingResponse = UpdateThingResponse'
    { _utrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsResponseStatus'
updateThingResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateThingResponse
updateThingResponse pResponseStatus_ =
    UpdateThingResponse'
    { _utrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
utrsResponseStatus :: Lens' UpdateThingResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a});

instance NFData UpdateThingResponse
