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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thing record in the registry.
--
--
module Network.AWS.IoT.CreateThing
    (
    -- * Creating a Request
      createThing
    , CreateThing
    -- * Request Lenses
    , ctThingTypeName
    , ctAttributePayload
    , ctThingName

    -- * Destructuring the Response
    , createThingResponse
    , CreateThingResponse
    -- * Response Lenses
    , ctrsThingARN
    , ctrsThingName
    , ctrsThingId
    , ctrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreateThing operation.
--
--
--
-- /See:/ 'createThing' smart constructor.
data CreateThing = CreateThing'
  { _ctThingTypeName    :: !(Maybe Text)
  , _ctAttributePayload :: !(Maybe AttributePayload)
  , _ctThingName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctThingTypeName' - The name of the thing type associated with the new thing.
--
-- * 'ctAttributePayload' - The attribute payload, which consists of up to three name/value pairs in a JSON document. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
--
-- * 'ctThingName' - The name of the thing to create.
createThing
    :: Text -- ^ 'ctThingName'
    -> CreateThing
createThing pThingName_ =
  CreateThing'
    { _ctThingTypeName = Nothing
    , _ctAttributePayload = Nothing
    , _ctThingName = pThingName_
    }


-- | The name of the thing type associated with the new thing.
ctThingTypeName :: Lens' CreateThing (Maybe Text)
ctThingTypeName = lens _ctThingTypeName (\ s a -> s{_ctThingTypeName = a})

-- | The attribute payload, which consists of up to three name/value pairs in a JSON document. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
ctAttributePayload :: Lens' CreateThing (Maybe AttributePayload)
ctAttributePayload = lens _ctAttributePayload (\ s a -> s{_ctAttributePayload = a})

-- | The name of the thing to create.
ctThingName :: Lens' CreateThing Text
ctThingName = lens _ctThingName (\ s a -> s{_ctThingName = a})

instance AWSRequest CreateThing where
        type Rs CreateThing = CreateThingResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateThingResponse' <$>
                   (x .?> "thingArn") <*> (x .?> "thingName") <*>
                     (x .?> "thingId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateThing where

instance NFData CreateThing where

instance ToHeaders CreateThing where
        toHeaders = const mempty

instance ToJSON CreateThing where
        toJSON CreateThing'{..}
          = object
              (catMaybes
                 [("thingTypeName" .=) <$> _ctThingTypeName,
                  ("attributePayload" .=) <$> _ctAttributePayload])

instance ToPath CreateThing where
        toPath CreateThing'{..}
          = mconcat ["/things/", toBS _ctThingName]

instance ToQuery CreateThing where
        toQuery = const mempty

-- | The output of the CreateThing operation.
--
--
--
-- /See:/ 'createThingResponse' smart constructor.
data CreateThingResponse = CreateThingResponse'
  { _ctrsThingARN       :: !(Maybe Text)
  , _ctrsThingName      :: !(Maybe Text)
  , _ctrsThingId        :: !(Maybe Text)
  , _ctrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsThingARN' - The ARN of the new thing.
--
-- * 'ctrsThingName' - The name of the new thing.
--
-- * 'ctrsThingId' - The thing ID.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createThingResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateThingResponse
createThingResponse pResponseStatus_ =
  CreateThingResponse'
    { _ctrsThingARN = Nothing
    , _ctrsThingName = Nothing
    , _ctrsThingId = Nothing
    , _ctrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the new thing.
ctrsThingARN :: Lens' CreateThingResponse (Maybe Text)
ctrsThingARN = lens _ctrsThingARN (\ s a -> s{_ctrsThingARN = a})

-- | The name of the new thing.
ctrsThingName :: Lens' CreateThingResponse (Maybe Text)
ctrsThingName = lens _ctrsThingName (\ s a -> s{_ctrsThingName = a})

-- | The thing ID.
ctrsThingId :: Lens' CreateThingResponse (Maybe Text)
ctrsThingId = lens _ctrsThingId (\ s a -> s{_ctrsThingId = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateThingResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateThingResponse where
