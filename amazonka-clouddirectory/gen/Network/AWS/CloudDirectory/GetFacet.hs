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
-- Module      : Network.AWS.CloudDirectory.GetFacet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of the 'Facet' , such as facet name, attributes, 'Rule' s, or @ObjectType@ . You can call this on all kinds of schema facets -- published, development, or applied.
--
--
module Network.AWS.CloudDirectory.GetFacet
    (
    -- * Creating a Request
      getFacet
    , GetFacet
    -- * Request Lenses
    , gfSchemaARN
    , gfName

    -- * Destructuring the Response
    , getFacetResponse
    , GetFacetResponse
    -- * Response Lenses
    , gfrsFacet
    , gfrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFacet' smart constructor.
data GetFacet = GetFacet'
  { _gfSchemaARN :: !Text
  , _gfName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfSchemaARN' - The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- * 'gfName' - The name of the facet to retrieve.
getFacet
    :: Text -- ^ 'gfSchemaARN'
    -> Text -- ^ 'gfName'
    -> GetFacet
getFacet pSchemaARN_ pName_ =
  GetFacet' {_gfSchemaARN = pSchemaARN_, _gfName = pName_}


-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
gfSchemaARN :: Lens' GetFacet Text
gfSchemaARN = lens _gfSchemaARN (\ s a -> s{_gfSchemaARN = a})

-- | The name of the facet to retrieve.
gfName :: Lens' GetFacet Text
gfName = lens _gfName (\ s a -> s{_gfName = a})

instance AWSRequest GetFacet where
        type Rs GetFacet = GetFacetResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetFacetResponse' <$>
                   (x .?> "Facet") <*> (pure (fromEnum s)))

instance Hashable GetFacet where

instance NFData GetFacet where

instance ToHeaders GetFacet where
        toHeaders GetFacet'{..}
          = mconcat ["x-amz-data-partition" =# _gfSchemaARN]

instance ToJSON GetFacet where
        toJSON GetFacet'{..}
          = object (catMaybes [Just ("Name" .= _gfName)])

instance ToPath GetFacet where
        toPath
          = const "/amazonclouddirectory/2017-01-11/facet"

instance ToQuery GetFacet where
        toQuery = const mempty

-- | /See:/ 'getFacetResponse' smart constructor.
data GetFacetResponse = GetFacetResponse'
  { _gfrsFacet          :: !(Maybe Facet)
  , _gfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFacetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsFacet' - The 'Facet' structure that is associated with the facet.
--
-- * 'gfrsResponseStatus' - -- | The response status code.
getFacetResponse
    :: Int -- ^ 'gfrsResponseStatus'
    -> GetFacetResponse
getFacetResponse pResponseStatus_ =
  GetFacetResponse'
    {_gfrsFacet = Nothing, _gfrsResponseStatus = pResponseStatus_}


-- | The 'Facet' structure that is associated with the facet.
gfrsFacet :: Lens' GetFacetResponse (Maybe Facet)
gfrsFacet = lens _gfrsFacet (\ s a -> s{_gfrsFacet = a})

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFacetResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\ s a -> s{_gfrsResponseStatus = a})

instance NFData GetFacetResponse where
