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
-- Module      : Network.AWS.WorkDocs.DeleteCustomMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes custom metadata from the specified resource.
--
--
module Network.AWS.WorkDocs.DeleteCustomMetadata
    (
    -- * Creating a Request
      deleteCustomMetadata
    , DeleteCustomMetadata
    -- * Request Lenses
    , dcmVersionId
    , dcmDeleteAll
    , dcmAuthenticationToken
    , dcmKeys
    , dcmResourceId

    -- * Destructuring the Response
    , deleteCustomMetadataResponse
    , DeleteCustomMetadataResponse
    -- * Response Lenses
    , dcmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { _dcmVersionId           :: !(Maybe Text)
  , _dcmDeleteAll           :: !(Maybe Bool)
  , _dcmAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dcmKeys                :: !(Maybe [Text])
  , _dcmResourceId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCustomMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcmVersionId' - The ID of the version, if the custom metadata is being deleted from a document version.
--
-- * 'dcmDeleteAll' - Flag to indicate removal of all custom metadata properties from the specified resource.
--
-- * 'dcmAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dcmKeys' - List of properties to remove.
--
-- * 'dcmResourceId' - The ID of the resource, either a document or folder.
deleteCustomMetadata
    :: Text -- ^ 'dcmResourceId'
    -> DeleteCustomMetadata
deleteCustomMetadata pResourceId_ =
  DeleteCustomMetadata'
    { _dcmVersionId = Nothing
    , _dcmDeleteAll = Nothing
    , _dcmAuthenticationToken = Nothing
    , _dcmKeys = Nothing
    , _dcmResourceId = pResourceId_
    }


-- | The ID of the version, if the custom metadata is being deleted from a document version.
dcmVersionId :: Lens' DeleteCustomMetadata (Maybe Text)
dcmVersionId = lens _dcmVersionId (\ s a -> s{_dcmVersionId = a})

-- | Flag to indicate removal of all custom metadata properties from the specified resource.
dcmDeleteAll :: Lens' DeleteCustomMetadata (Maybe Bool)
dcmDeleteAll = lens _dcmDeleteAll (\ s a -> s{_dcmDeleteAll = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dcmAuthenticationToken :: Lens' DeleteCustomMetadata (Maybe Text)
dcmAuthenticationToken = lens _dcmAuthenticationToken (\ s a -> s{_dcmAuthenticationToken = a}) . mapping _Sensitive

-- | List of properties to remove.
dcmKeys :: Lens' DeleteCustomMetadata [Text]
dcmKeys = lens _dcmKeys (\ s a -> s{_dcmKeys = a}) . _Default . _Coerce

-- | The ID of the resource, either a document or folder.
dcmResourceId :: Lens' DeleteCustomMetadata Text
dcmResourceId = lens _dcmResourceId (\ s a -> s{_dcmResourceId = a})

instance AWSRequest DeleteCustomMetadata where
        type Rs DeleteCustomMetadata =
             DeleteCustomMetadataResponse
        request = delete workDocs
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteCustomMetadataResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteCustomMetadata where

instance NFData DeleteCustomMetadata where

instance ToHeaders DeleteCustomMetadata where
        toHeaders DeleteCustomMetadata'{..}
          = mconcat
              ["Authentication" =# _dcmAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeleteCustomMetadata where
        toPath DeleteCustomMetadata'{..}
          = mconcat
              ["/api/v1/resources/", toBS _dcmResourceId,
               "/customMetadata"]

instance ToQuery DeleteCustomMetadata where
        toQuery DeleteCustomMetadata'{..}
          = mconcat
              ["versionId" =: _dcmVersionId,
               "deleteAll" =: _dcmDeleteAll,
               "keys" =:
                 toQuery (toQueryList "member" <$> _dcmKeys)]

-- | /See:/ 'deleteCustomMetadataResponse' smart constructor.
newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { _dcmrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCustomMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcmrsResponseStatus' - -- | The response status code.
deleteCustomMetadataResponse
    :: Int -- ^ 'dcmrsResponseStatus'
    -> DeleteCustomMetadataResponse
deleteCustomMetadataResponse pResponseStatus_ =
  DeleteCustomMetadataResponse' {_dcmrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcmrsResponseStatus :: Lens' DeleteCustomMetadataResponse Int
dcmrsResponseStatus = lens _dcmrsResponseStatus (\ s a -> s{_dcmrsResponseStatus = a})

instance NFData DeleteCustomMetadataResponse where
