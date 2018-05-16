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
-- Module      : Network.AWS.WorkDocs.CreateCustomMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more custom properties to the specified resource (a folder, document, or version).
--
--
module Network.AWS.WorkDocs.CreateCustomMetadata
    (
    -- * Creating a Request
      createCustomMetadata
    , CreateCustomMetadata
    -- * Request Lenses
    , ccmVersionId
    , ccmAuthenticationToken
    , ccmResourceId
    , ccmCustomMetadata

    -- * Destructuring the Response
    , createCustomMetadataResponse
    , CreateCustomMetadataResponse
    -- * Response Lenses
    , ccmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'createCustomMetadata' smart constructor.
data CreateCustomMetadata = CreateCustomMetadata'
  { _ccmVersionId           :: !(Maybe Text)
  , _ccmAuthenticationToken :: !(Maybe (Sensitive Text))
  , _ccmResourceId          :: !Text
  , _ccmCustomMetadata      :: !(Map Text Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCustomMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccmVersionId' - The ID of the version, if the custom metadata is being added to a document version.
--
-- * 'ccmAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'ccmResourceId' - The ID of the resource.
--
-- * 'ccmCustomMetadata' - Custom metadata in the form of name-value pairs.
createCustomMetadata
    :: Text -- ^ 'ccmResourceId'
    -> CreateCustomMetadata
createCustomMetadata pResourceId_ =
  CreateCustomMetadata'
    { _ccmVersionId = Nothing
    , _ccmAuthenticationToken = Nothing
    , _ccmResourceId = pResourceId_
    , _ccmCustomMetadata = mempty
    }


-- | The ID of the version, if the custom metadata is being added to a document version.
ccmVersionId :: Lens' CreateCustomMetadata (Maybe Text)
ccmVersionId = lens _ccmVersionId (\ s a -> s{_ccmVersionId = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
ccmAuthenticationToken :: Lens' CreateCustomMetadata (Maybe Text)
ccmAuthenticationToken = lens _ccmAuthenticationToken (\ s a -> s{_ccmAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the resource.
ccmResourceId :: Lens' CreateCustomMetadata Text
ccmResourceId = lens _ccmResourceId (\ s a -> s{_ccmResourceId = a})

-- | Custom metadata in the form of name-value pairs.
ccmCustomMetadata :: Lens' CreateCustomMetadata (HashMap Text Text)
ccmCustomMetadata = lens _ccmCustomMetadata (\ s a -> s{_ccmCustomMetadata = a}) . _Map

instance AWSRequest CreateCustomMetadata where
        type Rs CreateCustomMetadata =
             CreateCustomMetadataResponse
        request = putJSON workDocs
        response
          = receiveEmpty
              (\ s h x ->
                 CreateCustomMetadataResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateCustomMetadata where

instance NFData CreateCustomMetadata where

instance ToHeaders CreateCustomMetadata where
        toHeaders CreateCustomMetadata'{..}
          = mconcat
              ["Authentication" =# _ccmAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateCustomMetadata where
        toJSON CreateCustomMetadata'{..}
          = object
              (catMaybes
                 [Just ("CustomMetadata" .= _ccmCustomMetadata)])

instance ToPath CreateCustomMetadata where
        toPath CreateCustomMetadata'{..}
          = mconcat
              ["/api/v1/resources/", toBS _ccmResourceId,
               "/customMetadata"]

instance ToQuery CreateCustomMetadata where
        toQuery CreateCustomMetadata'{..}
          = mconcat ["versionid" =: _ccmVersionId]

-- | /See:/ 'createCustomMetadataResponse' smart constructor.
newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse'
  { _ccmrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCustomMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccmrsResponseStatus' - -- | The response status code.
createCustomMetadataResponse
    :: Int -- ^ 'ccmrsResponseStatus'
    -> CreateCustomMetadataResponse
createCustomMetadataResponse pResponseStatus_ =
  CreateCustomMetadataResponse' {_ccmrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccmrsResponseStatus :: Lens' CreateCustomMetadataResponse Int
ccmrsResponseStatus = lens _ccmrsResponseStatus (\ s a -> s{_ccmrsResponseStatus = a})

instance NFData CreateCustomMetadataResponse where
