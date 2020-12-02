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
-- Module      : Network.AWS.CloudDirectory.CreateFacet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'Facet' in a schema. Facet creation is allowed only in development or applied schemas.
--
--
module Network.AWS.CloudDirectory.CreateFacet
    (
    -- * Creating a Request
      createFacet
    , CreateFacet
    -- * Request Lenses
    , cfAttributes
    , cfSchemaARN
    , cfName
    , cfObjectType

    -- * Destructuring the Response
    , createFacetResponse
    , CreateFacetResponse
    -- * Response Lenses
    , cfrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFacet' smart constructor.
data CreateFacet = CreateFacet'
  { _cfAttributes :: !(Maybe [FacetAttribute])
  , _cfSchemaARN  :: !Text
  , _cfName       :: !Text
  , _cfObjectType :: !ObjectType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfAttributes' - The attributes that are associated with the 'Facet' .
--
-- * 'cfSchemaARN' - The schema ARN in which the new 'Facet' will be created. For more information, see 'arns' .
--
-- * 'cfName' - The name of the 'Facet' , which is unique for a given schema.
--
-- * 'cfObjectType' - Specifies whether a given object created from this facet is of type node, leaf node, policy or index.     * Node: Can have multiple children but one parent.     * Leaf node: Cannot have children but can have multiple parents.     * Policy: Allows you to store a policy document and policy type. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .     * Index: Can be created with the Index API.
createFacet
    :: Text -- ^ 'cfSchemaARN'
    -> Text -- ^ 'cfName'
    -> ObjectType -- ^ 'cfObjectType'
    -> CreateFacet
createFacet pSchemaARN_ pName_ pObjectType_ =
  CreateFacet'
    { _cfAttributes = Nothing
    , _cfSchemaARN = pSchemaARN_
    , _cfName = pName_
    , _cfObjectType = pObjectType_
    }


-- | The attributes that are associated with the 'Facet' .
cfAttributes :: Lens' CreateFacet [FacetAttribute]
cfAttributes = lens _cfAttributes (\ s a -> s{_cfAttributes = a}) . _Default . _Coerce

-- | The schema ARN in which the new 'Facet' will be created. For more information, see 'arns' .
cfSchemaARN :: Lens' CreateFacet Text
cfSchemaARN = lens _cfSchemaARN (\ s a -> s{_cfSchemaARN = a})

-- | The name of the 'Facet' , which is unique for a given schema.
cfName :: Lens' CreateFacet Text
cfName = lens _cfName (\ s a -> s{_cfName = a})

-- | Specifies whether a given object created from this facet is of type node, leaf node, policy or index.     * Node: Can have multiple children but one parent.     * Leaf node: Cannot have children but can have multiple parents.     * Policy: Allows you to store a policy document and policy type. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .     * Index: Can be created with the Index API.
cfObjectType :: Lens' CreateFacet ObjectType
cfObjectType = lens _cfObjectType (\ s a -> s{_cfObjectType = a})

instance AWSRequest CreateFacet where
        type Rs CreateFacet = CreateFacetResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 CreateFacetResponse' <$> (pure (fromEnum s)))

instance Hashable CreateFacet where

instance NFData CreateFacet where

instance ToHeaders CreateFacet where
        toHeaders CreateFacet'{..}
          = mconcat ["x-amz-data-partition" =# _cfSchemaARN]

instance ToJSON CreateFacet where
        toJSON CreateFacet'{..}
          = object
              (catMaybes
                 [("Attributes" .=) <$> _cfAttributes,
                  Just ("Name" .= _cfName),
                  Just ("ObjectType" .= _cfObjectType)])

instance ToPath CreateFacet where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/facet/create"

instance ToQuery CreateFacet where
        toQuery = const mempty

-- | /See:/ 'createFacetResponse' smart constructor.
newtype CreateFacetResponse = CreateFacetResponse'
  { _cfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFacetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFacetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFacetResponse
createFacetResponse pResponseStatus_ =
  CreateFacetResponse' {_cfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFacetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFacetResponse where
