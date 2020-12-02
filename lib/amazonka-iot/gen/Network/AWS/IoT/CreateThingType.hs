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
-- Module      : Network.AWS.IoT.CreateThingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new thing type.
--
--
module Network.AWS.IoT.CreateThingType
    (
    -- * Creating a Request
      createThingType
    , CreateThingType
    -- * Request Lenses
    , cttThingTypeProperties
    , cttThingTypeName

    -- * Destructuring the Response
    , createThingTypeResponse
    , CreateThingTypeResponse
    -- * Response Lenses
    , cttrsThingTypeName
    , cttrsThingTypeId
    , cttrsThingTypeARN
    , cttrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreateThingType operation.
--
--
--
-- /See:/ 'createThingType' smart constructor.
data CreateThingType = CreateThingType'
  { _cttThingTypeProperties :: !(Maybe ThingTypeProperties)
  , _cttThingTypeName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cttThingTypeProperties' - The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
--
-- * 'cttThingTypeName' - The name of the thing type.
createThingType
    :: Text -- ^ 'cttThingTypeName'
    -> CreateThingType
createThingType pThingTypeName_ =
  CreateThingType'
    {_cttThingTypeProperties = Nothing, _cttThingTypeName = pThingTypeName_}


-- | The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
cttThingTypeProperties :: Lens' CreateThingType (Maybe ThingTypeProperties)
cttThingTypeProperties = lens _cttThingTypeProperties (\ s a -> s{_cttThingTypeProperties = a})

-- | The name of the thing type.
cttThingTypeName :: Lens' CreateThingType Text
cttThingTypeName = lens _cttThingTypeName (\ s a -> s{_cttThingTypeName = a})

instance AWSRequest CreateThingType where
        type Rs CreateThingType = CreateThingTypeResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateThingTypeResponse' <$>
                   (x .?> "thingTypeName") <*> (x .?> "thingTypeId") <*>
                     (x .?> "thingTypeArn")
                     <*> (pure (fromEnum s)))

instance Hashable CreateThingType where

instance NFData CreateThingType where

instance ToHeaders CreateThingType where
        toHeaders = const mempty

instance ToJSON CreateThingType where
        toJSON CreateThingType'{..}
          = object
              (catMaybes
                 [("thingTypeProperties" .=) <$>
                    _cttThingTypeProperties])

instance ToPath CreateThingType where
        toPath CreateThingType'{..}
          = mconcat ["/thing-types/", toBS _cttThingTypeName]

instance ToQuery CreateThingType where
        toQuery = const mempty

-- | The output of the CreateThingType operation.
--
--
--
-- /See:/ 'createThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { _cttrsThingTypeName  :: !(Maybe Text)
  , _cttrsThingTypeId    :: !(Maybe Text)
  , _cttrsThingTypeARN   :: !(Maybe Text)
  , _cttrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cttrsThingTypeName' - The name of the thing type.
--
-- * 'cttrsThingTypeId' - The thing type ID.
--
-- * 'cttrsThingTypeARN' - The Amazon Resource Name (ARN) of the thing type.
--
-- * 'cttrsResponseStatus' - -- | The response status code.
createThingTypeResponse
    :: Int -- ^ 'cttrsResponseStatus'
    -> CreateThingTypeResponse
createThingTypeResponse pResponseStatus_ =
  CreateThingTypeResponse'
    { _cttrsThingTypeName = Nothing
    , _cttrsThingTypeId = Nothing
    , _cttrsThingTypeARN = Nothing
    , _cttrsResponseStatus = pResponseStatus_
    }


-- | The name of the thing type.
cttrsThingTypeName :: Lens' CreateThingTypeResponse (Maybe Text)
cttrsThingTypeName = lens _cttrsThingTypeName (\ s a -> s{_cttrsThingTypeName = a})

-- | The thing type ID.
cttrsThingTypeId :: Lens' CreateThingTypeResponse (Maybe Text)
cttrsThingTypeId = lens _cttrsThingTypeId (\ s a -> s{_cttrsThingTypeId = a})

-- | The Amazon Resource Name (ARN) of the thing type.
cttrsThingTypeARN :: Lens' CreateThingTypeResponse (Maybe Text)
cttrsThingTypeARN = lens _cttrsThingTypeARN (\ s a -> s{_cttrsThingTypeARN = a})

-- | -- | The response status code.
cttrsResponseStatus :: Lens' CreateThingTypeResponse Int
cttrsResponseStatus = lens _cttrsResponseStatus (\ s a -> s{_cttrsResponseStatus = a})

instance NFData CreateThingTypeResponse where
