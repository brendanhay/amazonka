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
-- Module      : Network.AWS.CloudDirectory.UpdateTypedLinkFacet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'TypedLinkFacet' . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.UpdateTypedLinkFacet
    (
    -- * Creating a Request
      updateTypedLinkFacet
    , UpdateTypedLinkFacet
    -- * Request Lenses
    , utlfSchemaARN
    , utlfName
    , utlfAttributeUpdates
    , utlfIdentityAttributeOrder

    -- * Destructuring the Response
    , updateTypedLinkFacetResponse
    , UpdateTypedLinkFacetResponse
    -- * Response Lenses
    , utlfrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTypedLinkFacet' smart constructor.
data UpdateTypedLinkFacet = UpdateTypedLinkFacet'
  { _utlfSchemaARN              :: !Text
  , _utlfName                   :: !Text
  , _utlfAttributeUpdates       :: ![TypedLinkFacetAttributeUpdate]
  , _utlfIdentityAttributeOrder :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTypedLinkFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utlfSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- * 'utlfName' - The unique name of the typed link facet.
--
-- * 'utlfAttributeUpdates' - Attributes update structure.
--
-- * 'utlfIdentityAttributeOrder' - The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to a typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
updateTypedLinkFacet
    :: Text -- ^ 'utlfSchemaARN'
    -> Text -- ^ 'utlfName'
    -> UpdateTypedLinkFacet
updateTypedLinkFacet pSchemaARN_ pName_ =
  UpdateTypedLinkFacet'
    { _utlfSchemaARN = pSchemaARN_
    , _utlfName = pName_
    , _utlfAttributeUpdates = mempty
    , _utlfIdentityAttributeOrder = mempty
    }


-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
utlfSchemaARN :: Lens' UpdateTypedLinkFacet Text
utlfSchemaARN = lens _utlfSchemaARN (\ s a -> s{_utlfSchemaARN = a})

-- | The unique name of the typed link facet.
utlfName :: Lens' UpdateTypedLinkFacet Text
utlfName = lens _utlfName (\ s a -> s{_utlfName = a})

-- | Attributes update structure.
utlfAttributeUpdates :: Lens' UpdateTypedLinkFacet [TypedLinkFacetAttributeUpdate]
utlfAttributeUpdates = lens _utlfAttributeUpdates (\ s a -> s{_utlfAttributeUpdates = a}) . _Coerce

-- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to a typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
utlfIdentityAttributeOrder :: Lens' UpdateTypedLinkFacet [Text]
utlfIdentityAttributeOrder = lens _utlfIdentityAttributeOrder (\ s a -> s{_utlfIdentityAttributeOrder = a}) . _Coerce

instance AWSRequest UpdateTypedLinkFacet where
        type Rs UpdateTypedLinkFacet =
             UpdateTypedLinkFacetResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateTypedLinkFacetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateTypedLinkFacet where

instance NFData UpdateTypedLinkFacet where

instance ToHeaders UpdateTypedLinkFacet where
        toHeaders UpdateTypedLinkFacet'{..}
          = mconcat ["x-amz-data-partition" =# _utlfSchemaARN]

instance ToJSON UpdateTypedLinkFacet where
        toJSON UpdateTypedLinkFacet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _utlfName),
                  Just ("AttributeUpdates" .= _utlfAttributeUpdates),
                  Just
                    ("IdentityAttributeOrder" .=
                       _utlfIdentityAttributeOrder)])

instance ToPath UpdateTypedLinkFacet where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/facet"

instance ToQuery UpdateTypedLinkFacet where
        toQuery = const mempty

-- | /See:/ 'updateTypedLinkFacetResponse' smart constructor.
newtype UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse'
  { _utlfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTypedLinkFacetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utlfrsResponseStatus' - -- | The response status code.
updateTypedLinkFacetResponse
    :: Int -- ^ 'utlfrsResponseStatus'
    -> UpdateTypedLinkFacetResponse
updateTypedLinkFacetResponse pResponseStatus_ =
  UpdateTypedLinkFacetResponse' {_utlfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
utlfrsResponseStatus :: Lens' UpdateTypedLinkFacetResponse Int
utlfrsResponseStatus = lens _utlfrsResponseStatus (\ s a -> s{_utlfrsResponseStatus = a})

instance NFData UpdateTypedLinkFacetResponse where
