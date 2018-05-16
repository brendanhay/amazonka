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
-- Module      : Network.AWS.CloudDirectory.RemoveFacetFromObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified facet from the specified object.
--
--
module Network.AWS.CloudDirectory.RemoveFacetFromObject
    (
    -- * Creating a Request
      removeFacetFromObject
    , RemoveFacetFromObject
    -- * Request Lenses
    , rffoDirectoryARN
    , rffoSchemaFacet
    , rffoObjectReference

    -- * Destructuring the Response
    , removeFacetFromObjectResponse
    , RemoveFacetFromObjectResponse
    -- * Response Lenses
    , rfforsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeFacetFromObject' smart constructor.
data RemoveFacetFromObject = RemoveFacetFromObject'
  { _rffoDirectoryARN    :: !Text
  , _rffoSchemaFacet     :: !SchemaFacet
  , _rffoObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveFacetFromObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rffoDirectoryARN' - The ARN of the directory in which the object resides.
--
-- * 'rffoSchemaFacet' - The facet to remove. See 'SchemaFacet' for details.
--
-- * 'rffoObjectReference' - A reference to the object to remove the facet from.
removeFacetFromObject
    :: Text -- ^ 'rffoDirectoryARN'
    -> SchemaFacet -- ^ 'rffoSchemaFacet'
    -> ObjectReference -- ^ 'rffoObjectReference'
    -> RemoveFacetFromObject
removeFacetFromObject pDirectoryARN_ pSchemaFacet_ pObjectReference_ =
  RemoveFacetFromObject'
    { _rffoDirectoryARN = pDirectoryARN_
    , _rffoSchemaFacet = pSchemaFacet_
    , _rffoObjectReference = pObjectReference_
    }


-- | The ARN of the directory in which the object resides.
rffoDirectoryARN :: Lens' RemoveFacetFromObject Text
rffoDirectoryARN = lens _rffoDirectoryARN (\ s a -> s{_rffoDirectoryARN = a})

-- | The facet to remove. See 'SchemaFacet' for details.
rffoSchemaFacet :: Lens' RemoveFacetFromObject SchemaFacet
rffoSchemaFacet = lens _rffoSchemaFacet (\ s a -> s{_rffoSchemaFacet = a})

-- | A reference to the object to remove the facet from.
rffoObjectReference :: Lens' RemoveFacetFromObject ObjectReference
rffoObjectReference = lens _rffoObjectReference (\ s a -> s{_rffoObjectReference = a})

instance AWSRequest RemoveFacetFromObject where
        type Rs RemoveFacetFromObject =
             RemoveFacetFromObjectResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveFacetFromObjectResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RemoveFacetFromObject where

instance NFData RemoveFacetFromObject where

instance ToHeaders RemoveFacetFromObject where
        toHeaders RemoveFacetFromObject'{..}
          = mconcat
              ["x-amz-data-partition" =# _rffoDirectoryARN]

instance ToJSON RemoveFacetFromObject where
        toJSON RemoveFacetFromObject'{..}
          = object
              (catMaybes
                 [Just ("SchemaFacet" .= _rffoSchemaFacet),
                  Just ("ObjectReference" .= _rffoObjectReference)])

instance ToPath RemoveFacetFromObject where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/facets/delete"

instance ToQuery RemoveFacetFromObject where
        toQuery = const mempty

-- | /See:/ 'removeFacetFromObjectResponse' smart constructor.
newtype RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse'
  { _rfforsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveFacetFromObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfforsResponseStatus' - -- | The response status code.
removeFacetFromObjectResponse
    :: Int -- ^ 'rfforsResponseStatus'
    -> RemoveFacetFromObjectResponse
removeFacetFromObjectResponse pResponseStatus_ =
  RemoveFacetFromObjectResponse' {_rfforsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rfforsResponseStatus :: Lens' RemoveFacetFromObjectResponse Int
rfforsResponseStatus = lens _rfforsResponseStatus (\ s a -> s{_rfforsResponseStatus = a})

instance NFData RemoveFacetFromObjectResponse where
