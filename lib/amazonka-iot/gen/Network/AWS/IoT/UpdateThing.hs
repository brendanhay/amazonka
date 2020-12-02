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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a thing.
--
--
module Network.AWS.IoT.UpdateThing
    (
    -- * Creating a Request
      updateThing
    , UpdateThing
    -- * Request Lenses
    , utRemoveThingType
    , utThingTypeName
    , utExpectedVersion
    , utAttributePayload
    , utThingName

    -- * Destructuring the Response
    , updateThingResponse
    , UpdateThingResponse
    -- * Response Lenses
    , utrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the UpdateThing operation.
--
--
--
-- /See:/ 'updateThing' smart constructor.
data UpdateThing = UpdateThing'
  { _utRemoveThingType  :: !(Maybe Bool)
  , _utThingTypeName    :: !(Maybe Text)
  , _utExpectedVersion  :: !(Maybe Integer)
  , _utAttributePayload :: !(Maybe AttributePayload)
  , _utThingName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utRemoveThingType' - Remove a thing type association. If __true__ , the association is removed.
--
-- * 'utThingTypeName' - The name of the thing type.
--
-- * 'utExpectedVersion' - The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @UpdateThing@ request is rejected with a @VersionConflictException@ .
--
-- * 'utAttributePayload' - A list of thing attributes, a JSON string containing name-value pairs. For example: @{\"attributes\":{\"name1\":\"value2\"}}@  This data is used to add new attributes or update existing attributes.
--
-- * 'utThingName' - The name of the thing to update.
updateThing
    :: Text -- ^ 'utThingName'
    -> UpdateThing
updateThing pThingName_ =
  UpdateThing'
    { _utRemoveThingType = Nothing
    , _utThingTypeName = Nothing
    , _utExpectedVersion = Nothing
    , _utAttributePayload = Nothing
    , _utThingName = pThingName_
    }


-- | Remove a thing type association. If __true__ , the association is removed.
utRemoveThingType :: Lens' UpdateThing (Maybe Bool)
utRemoveThingType = lens _utRemoveThingType (\ s a -> s{_utRemoveThingType = a})

-- | The name of the thing type.
utThingTypeName :: Lens' UpdateThing (Maybe Text)
utThingTypeName = lens _utThingTypeName (\ s a -> s{_utThingTypeName = a})

-- | The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @UpdateThing@ request is rejected with a @VersionConflictException@ .
utExpectedVersion :: Lens' UpdateThing (Maybe Integer)
utExpectedVersion = lens _utExpectedVersion (\ s a -> s{_utExpectedVersion = a})

-- | A list of thing attributes, a JSON string containing name-value pairs. For example: @{\"attributes\":{\"name1\":\"value2\"}}@  This data is used to add new attributes or update existing attributes.
utAttributePayload :: Lens' UpdateThing (Maybe AttributePayload)
utAttributePayload = lens _utAttributePayload (\ s a -> s{_utAttributePayload = a})

-- | The name of the thing to update.
utThingName :: Lens' UpdateThing Text
utThingName = lens _utThingName (\ s a -> s{_utThingName = a})

instance AWSRequest UpdateThing where
        type Rs UpdateThing = UpdateThingResponse
        request = patchJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateThingResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateThing where

instance NFData UpdateThing where

instance ToHeaders UpdateThing where
        toHeaders = const mempty

instance ToJSON UpdateThing where
        toJSON UpdateThing'{..}
          = object
              (catMaybes
                 [("removeThingType" .=) <$> _utRemoveThingType,
                  ("thingTypeName" .=) <$> _utThingTypeName,
                  ("expectedVersion" .=) <$> _utExpectedVersion,
                  ("attributePayload" .=) <$> _utAttributePayload])

instance ToPath UpdateThing where
        toPath UpdateThing'{..}
          = mconcat ["/things/", toBS _utThingName]

instance ToQuery UpdateThing where
        toQuery = const mempty

-- | The output from the UpdateThing operation.
--
--
--
-- /See:/ 'updateThingResponse' smart constructor.
newtype UpdateThingResponse = UpdateThingResponse'
  { _utrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsResponseStatus' - -- | The response status code.
updateThingResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateThingResponse
updateThingResponse pResponseStatus_ =
  UpdateThingResponse' {_utrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
utrsResponseStatus :: Lens' UpdateThingResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a})

instance NFData UpdateThingResponse where
