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
-- Module      : Network.AWS.CloudDirectory.UpdateFacet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Does the following:
--
--
--     * Adds new @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--     * Updates existing @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--     * Deletes existing @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--
--
module Network.AWS.CloudDirectory.UpdateFacet
    (
    -- * Creating a Request
      updateFacet
    , UpdateFacet
    -- * Request Lenses
    , ufObjectType
    , ufAttributeUpdates
    , ufSchemaARN
    , ufName

    -- * Destructuring the Response
    , updateFacetResponse
    , UpdateFacetResponse
    -- * Response Lenses
    , ufrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFacet' smart constructor.
data UpdateFacet = UpdateFacet'
  { _ufObjectType       :: !(Maybe ObjectType)
  , _ufAttributeUpdates :: !(Maybe [FacetAttributeUpdate])
  , _ufSchemaARN        :: !Text
  , _ufName             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufObjectType' - The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- * 'ufAttributeUpdates' - List of attributes that need to be updated in a given schema 'Facet' . Each attribute is followed by @AttributeAction@ , which specifies the type of update operation to perform.
--
-- * 'ufSchemaARN' - The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- * 'ufName' - The name of the facet.
updateFacet
    :: Text -- ^ 'ufSchemaARN'
    -> Text -- ^ 'ufName'
    -> UpdateFacet
updateFacet pSchemaARN_ pName_ =
  UpdateFacet'
    { _ufObjectType = Nothing
    , _ufAttributeUpdates = Nothing
    , _ufSchemaARN = pSchemaARN_
    , _ufName = pName_
    }


-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
ufObjectType :: Lens' UpdateFacet (Maybe ObjectType)
ufObjectType = lens _ufObjectType (\ s a -> s{_ufObjectType = a})

-- | List of attributes that need to be updated in a given schema 'Facet' . Each attribute is followed by @AttributeAction@ , which specifies the type of update operation to perform.
ufAttributeUpdates :: Lens' UpdateFacet [FacetAttributeUpdate]
ufAttributeUpdates = lens _ufAttributeUpdates (\ s a -> s{_ufAttributeUpdates = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
ufSchemaARN :: Lens' UpdateFacet Text
ufSchemaARN = lens _ufSchemaARN (\ s a -> s{_ufSchemaARN = a})

-- | The name of the facet.
ufName :: Lens' UpdateFacet Text
ufName = lens _ufName (\ s a -> s{_ufName = a})

instance AWSRequest UpdateFacet where
        type Rs UpdateFacet = UpdateFacetResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateFacetResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateFacet where

instance NFData UpdateFacet where

instance ToHeaders UpdateFacet where
        toHeaders UpdateFacet'{..}
          = mconcat ["x-amz-data-partition" =# _ufSchemaARN]

instance ToJSON UpdateFacet where
        toJSON UpdateFacet'{..}
          = object
              (catMaybes
                 [("ObjectType" .=) <$> _ufObjectType,
                  ("AttributeUpdates" .=) <$> _ufAttributeUpdates,
                  Just ("Name" .= _ufName)])

instance ToPath UpdateFacet where
        toPath
          = const "/amazonclouddirectory/2017-01-11/facet"

instance ToQuery UpdateFacet where
        toQuery = const mempty

-- | /See:/ 'updateFacetResponse' smart constructor.
newtype UpdateFacetResponse = UpdateFacetResponse'
  { _ufrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFacetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufrsResponseStatus' - -- | The response status code.
updateFacetResponse
    :: Int -- ^ 'ufrsResponseStatus'
    -> UpdateFacetResponse
updateFacetResponse pResponseStatus_ =
  UpdateFacetResponse' {_ufrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ufrsResponseStatus :: Lens' UpdateFacetResponse Int
ufrsResponseStatus = lens _ufrsResponseStatus (\ s a -> s{_ufrsResponseStatus = a})

instance NFData UpdateFacetResponse where
