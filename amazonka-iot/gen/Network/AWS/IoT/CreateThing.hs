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
-- Module      : Network.AWS.IoT.CreateThing
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thing in the Thing Registry.
module Network.AWS.IoT.CreateThing
    (
    -- * Creating a Request
      createThing
    , CreateThing
    -- * Request Lenses
    , ctAttributePayload
    , ctThingName

    -- * Destructuring the Response
    , createThingResponse
    , CreateThingResponse
    -- * Response Lenses
    , ctrsThingARN
    , ctrsThingName
    , ctrsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the CreateThing operation.
--
-- /See:/ 'createThing' smart constructor.
data CreateThing = CreateThing'
    { _ctAttributePayload :: !(Maybe AttributePayload)
    , _ctThingName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctAttributePayload'
--
-- * 'ctThingName'
createThing
    :: Text -- ^ 'ctThingName'
    -> CreateThing
createThing pThingName_ =
    CreateThing'
    { _ctAttributePayload = Nothing
    , _ctThingName = pThingName_
    }

-- | The attribute payload, which consists of up to 3 name\/value pairs in a
-- JSON document (for example,
-- {\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}).
ctAttributePayload :: Lens' CreateThing (Maybe AttributePayload)
ctAttributePayload = lens _ctAttributePayload (\ s a -> s{_ctAttributePayload = a});

-- | The name of the thing.
ctThingName :: Lens' CreateThing Text
ctThingName = lens _ctThingName (\ s a -> s{_ctThingName = a});

instance AWSRequest CreateThing where
        type Rs CreateThing = CreateThingResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateThingResponse' <$>
                   (x .?> "thingArn") <*> (x .?> "thingName") <*>
                     (pure (fromEnum s)))

instance Hashable CreateThing

instance ToHeaders CreateThing where
        toHeaders = const mempty

instance ToJSON CreateThing where
        toJSON CreateThing'{..}
          = object
              (catMaybes
                 [("attributePayload" .=) <$> _ctAttributePayload])

instance ToPath CreateThing where
        toPath CreateThing'{..}
          = mconcat ["/things/", toBS _ctThingName]

instance ToQuery CreateThing where
        toQuery = const mempty

-- | The output of the CreateThing operation.
--
-- /See:/ 'createThingResponse' smart constructor.
data CreateThingResponse = CreateThingResponse'
    { _ctrsThingARN       :: !(Maybe Text)
    , _ctrsThingName      :: !(Maybe Text)
    , _ctrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsThingARN'
--
-- * 'ctrsThingName'
--
-- * 'ctrsResponseStatus'
createThingResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateThingResponse
createThingResponse pResponseStatus_ =
    CreateThingResponse'
    { _ctrsThingARN = Nothing
    , _ctrsThingName = Nothing
    , _ctrsResponseStatus = pResponseStatus_
    }

-- | The thing ARN.
ctrsThingARN :: Lens' CreateThingResponse (Maybe Text)
ctrsThingARN = lens _ctrsThingARN (\ s a -> s{_ctrsThingARN = a});

-- | The name of the thing.
ctrsThingName :: Lens' CreateThingResponse (Maybe Text)
ctrsThingName = lens _ctrsThingName (\ s a -> s{_ctrsThingName = a});

-- | The response status code.
ctrsResponseStatus :: Lens' CreateThingResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a});
