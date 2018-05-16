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
-- Module      : Network.AWS.CloudDirectory.DeleteFacet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given 'Facet' . All attributes and 'Rule' s that are associated with the facet will be deleted. Only development schema facets are allowed deletion.
--
--
module Network.AWS.CloudDirectory.DeleteFacet
    (
    -- * Creating a Request
      deleteFacet
    , DeleteFacet
    -- * Request Lenses
    , dfSchemaARN
    , dfName

    -- * Destructuring the Response
    , deleteFacetResponse
    , DeleteFacetResponse
    -- * Response Lenses
    , dfrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFacet' smart constructor.
data DeleteFacet = DeleteFacet'
  { _dfSchemaARN :: !Text
  , _dfName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfSchemaARN' - The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- * 'dfName' - The name of the facet to delete.
deleteFacet
    :: Text -- ^ 'dfSchemaARN'
    -> Text -- ^ 'dfName'
    -> DeleteFacet
deleteFacet pSchemaARN_ pName_ =
  DeleteFacet' {_dfSchemaARN = pSchemaARN_, _dfName = pName_}


-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
dfSchemaARN :: Lens' DeleteFacet Text
dfSchemaARN = lens _dfSchemaARN (\ s a -> s{_dfSchemaARN = a})

-- | The name of the facet to delete.
dfName :: Lens' DeleteFacet Text
dfName = lens _dfName (\ s a -> s{_dfName = a})

instance AWSRequest DeleteFacet where
        type Rs DeleteFacet = DeleteFacetResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteFacetResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteFacet where

instance NFData DeleteFacet where

instance ToHeaders DeleteFacet where
        toHeaders DeleteFacet'{..}
          = mconcat ["x-amz-data-partition" =# _dfSchemaARN]

instance ToJSON DeleteFacet where
        toJSON DeleteFacet'{..}
          = object (catMaybes [Just ("Name" .= _dfName)])

instance ToPath DeleteFacet where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/facet/delete"

instance ToQuery DeleteFacet where
        toQuery = const mempty

-- | /See:/ 'deleteFacetResponse' smart constructor.
newtype DeleteFacetResponse = DeleteFacetResponse'
  { _dfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFacetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsResponseStatus' - -- | The response status code.
deleteFacetResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DeleteFacetResponse
deleteFacetResponse pResponseStatus_ =
  DeleteFacetResponse' {_dfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dfrsResponseStatus :: Lens' DeleteFacetResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

instance NFData DeleteFacetResponse where
