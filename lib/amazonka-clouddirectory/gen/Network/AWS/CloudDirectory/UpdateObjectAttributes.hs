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
-- Module      : Network.AWS.CloudDirectory.UpdateObjectAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given object's attributes.
--
--
module Network.AWS.CloudDirectory.UpdateObjectAttributes
    (
    -- * Creating a Request
      updateObjectAttributes
    , UpdateObjectAttributes
    -- * Request Lenses
    , uoaDirectoryARN
    , uoaObjectReference
    , uoaAttributeUpdates

    -- * Destructuring the Response
    , updateObjectAttributesResponse
    , UpdateObjectAttributesResponse
    -- * Response Lenses
    , uoarsObjectIdentifier
    , uoarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateObjectAttributes' smart constructor.
data UpdateObjectAttributes = UpdateObjectAttributes'
  { _uoaDirectoryARN     :: !Text
  , _uoaObjectReference  :: !ObjectReference
  , _uoaAttributeUpdates :: ![ObjectAttributeUpdate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoaDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- * 'uoaObjectReference' - The reference that identifies the object.
--
-- * 'uoaAttributeUpdates' - The attributes update structure.
updateObjectAttributes
    :: Text -- ^ 'uoaDirectoryARN'
    -> ObjectReference -- ^ 'uoaObjectReference'
    -> UpdateObjectAttributes
updateObjectAttributes pDirectoryARN_ pObjectReference_ =
  UpdateObjectAttributes'
    { _uoaDirectoryARN = pDirectoryARN_
    , _uoaObjectReference = pObjectReference_
    , _uoaAttributeUpdates = mempty
    }


-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
uoaDirectoryARN :: Lens' UpdateObjectAttributes Text
uoaDirectoryARN = lens _uoaDirectoryARN (\ s a -> s{_uoaDirectoryARN = a})

-- | The reference that identifies the object.
uoaObjectReference :: Lens' UpdateObjectAttributes ObjectReference
uoaObjectReference = lens _uoaObjectReference (\ s a -> s{_uoaObjectReference = a})

-- | The attributes update structure.
uoaAttributeUpdates :: Lens' UpdateObjectAttributes [ObjectAttributeUpdate]
uoaAttributeUpdates = lens _uoaAttributeUpdates (\ s a -> s{_uoaAttributeUpdates = a}) . _Coerce

instance AWSRequest UpdateObjectAttributes where
        type Rs UpdateObjectAttributes =
             UpdateObjectAttributesResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 UpdateObjectAttributesResponse' <$>
                   (x .?> "ObjectIdentifier") <*> (pure (fromEnum s)))

instance Hashable UpdateObjectAttributes where

instance NFData UpdateObjectAttributes where

instance ToHeaders UpdateObjectAttributes where
        toHeaders UpdateObjectAttributes'{..}
          = mconcat
              ["x-amz-data-partition" =# _uoaDirectoryARN]

instance ToJSON UpdateObjectAttributes where
        toJSON UpdateObjectAttributes'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _uoaObjectReference),
                  Just ("AttributeUpdates" .= _uoaAttributeUpdates)])

instance ToPath UpdateObjectAttributes where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/update"

instance ToQuery UpdateObjectAttributes where
        toQuery = const mempty

-- | /See:/ 'updateObjectAttributesResponse' smart constructor.
data UpdateObjectAttributesResponse = UpdateObjectAttributesResponse'
  { _uoarsObjectIdentifier :: !(Maybe Text)
  , _uoarsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoarsObjectIdentifier' - The @ObjectIdentifier@ of the updated object.
--
-- * 'uoarsResponseStatus' - -- | The response status code.
updateObjectAttributesResponse
    :: Int -- ^ 'uoarsResponseStatus'
    -> UpdateObjectAttributesResponse
updateObjectAttributesResponse pResponseStatus_ =
  UpdateObjectAttributesResponse'
    {_uoarsObjectIdentifier = Nothing, _uoarsResponseStatus = pResponseStatus_}


-- | The @ObjectIdentifier@ of the updated object.
uoarsObjectIdentifier :: Lens' UpdateObjectAttributesResponse (Maybe Text)
uoarsObjectIdentifier = lens _uoarsObjectIdentifier (\ s a -> s{_uoarsObjectIdentifier = a})

-- | -- | The response status code.
uoarsResponseStatus :: Lens' UpdateObjectAttributesResponse Int
uoarsResponseStatus = lens _uoarsResponseStatus (\ s a -> s{_uoarsResponseStatus = a})

instance NFData UpdateObjectAttributesResponse where
