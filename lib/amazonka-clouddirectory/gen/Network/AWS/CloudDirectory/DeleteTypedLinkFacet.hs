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
-- Module      : Network.AWS.CloudDirectory.DeleteTypedLinkFacet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'TypedLinkFacet' . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.DeleteTypedLinkFacet
    (
    -- * Creating a Request
      deleteTypedLinkFacet
    , DeleteTypedLinkFacet
    -- * Request Lenses
    , dtlfSchemaARN
    , dtlfName

    -- * Destructuring the Response
    , deleteTypedLinkFacetResponse
    , DeleteTypedLinkFacetResponse
    -- * Response Lenses
    , dtlfrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTypedLinkFacet' smart constructor.
data DeleteTypedLinkFacet = DeleteTypedLinkFacet'
  { _dtlfSchemaARN :: !Text
  , _dtlfName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTypedLinkFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtlfSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- * 'dtlfName' - The unique name of the typed link facet.
deleteTypedLinkFacet
    :: Text -- ^ 'dtlfSchemaARN'
    -> Text -- ^ 'dtlfName'
    -> DeleteTypedLinkFacet
deleteTypedLinkFacet pSchemaARN_ pName_ =
  DeleteTypedLinkFacet' {_dtlfSchemaARN = pSchemaARN_, _dtlfName = pName_}


-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
dtlfSchemaARN :: Lens' DeleteTypedLinkFacet Text
dtlfSchemaARN = lens _dtlfSchemaARN (\ s a -> s{_dtlfSchemaARN = a})

-- | The unique name of the typed link facet.
dtlfName :: Lens' DeleteTypedLinkFacet Text
dtlfName = lens _dtlfName (\ s a -> s{_dtlfName = a})

instance AWSRequest DeleteTypedLinkFacet where
        type Rs DeleteTypedLinkFacet =
             DeleteTypedLinkFacetResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTypedLinkFacetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteTypedLinkFacet where

instance NFData DeleteTypedLinkFacet where

instance ToHeaders DeleteTypedLinkFacet where
        toHeaders DeleteTypedLinkFacet'{..}
          = mconcat ["x-amz-data-partition" =# _dtlfSchemaARN]

instance ToJSON DeleteTypedLinkFacet where
        toJSON DeleteTypedLinkFacet'{..}
          = object (catMaybes [Just ("Name" .= _dtlfName)])

instance ToPath DeleteTypedLinkFacet where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/facet/delete"

instance ToQuery DeleteTypedLinkFacet where
        toQuery = const mempty

-- | /See:/ 'deleteTypedLinkFacetResponse' smart constructor.
newtype DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse'
  { _dtlfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTypedLinkFacetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtlfrsResponseStatus' - -- | The response status code.
deleteTypedLinkFacetResponse
    :: Int -- ^ 'dtlfrsResponseStatus'
    -> DeleteTypedLinkFacetResponse
deleteTypedLinkFacetResponse pResponseStatus_ =
  DeleteTypedLinkFacetResponse' {_dtlfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtlfrsResponseStatus :: Lens' DeleteTypedLinkFacetResponse Int
dtlfrsResponseStatus = lens _dtlfrsResponseStatus (\ s a -> s{_dtlfrsResponseStatus = a})

instance NFData DeleteTypedLinkFacetResponse where
