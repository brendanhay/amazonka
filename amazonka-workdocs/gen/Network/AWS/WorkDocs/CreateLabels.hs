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
-- Module      : Network.AWS.WorkDocs.CreateLabels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified list of labels to the given resource (a document or folder)
--
--
module Network.AWS.WorkDocs.CreateLabels
    (
    -- * Creating a Request
      createLabels
    , CreateLabels
    -- * Request Lenses
    , clAuthenticationToken
    , clResourceId
    , clLabels

    -- * Destructuring the Response
    , createLabelsResponse
    , CreateLabelsResponse
    -- * Response Lenses
    , clrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'createLabels' smart constructor.
data CreateLabels = CreateLabels'
  { _clAuthenticationToken :: !(Maybe (Sensitive Text))
  , _clResourceId          :: !Text
  , _clLabels              :: ![Text]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'clResourceId' - The ID of the resource.
--
-- * 'clLabels' - List of labels to add to the resource.
createLabels
    :: Text -- ^ 'clResourceId'
    -> CreateLabels
createLabels pResourceId_ =
  CreateLabels'
    { _clAuthenticationToken = Nothing
    , _clResourceId = pResourceId_
    , _clLabels = mempty
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
clAuthenticationToken :: Lens' CreateLabels (Maybe Text)
clAuthenticationToken = lens _clAuthenticationToken (\ s a -> s{_clAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the resource.
clResourceId :: Lens' CreateLabels Text
clResourceId = lens _clResourceId (\ s a -> s{_clResourceId = a})

-- | List of labels to add to the resource.
clLabels :: Lens' CreateLabels [Text]
clLabels = lens _clLabels (\ s a -> s{_clLabels = a}) . _Coerce

instance AWSRequest CreateLabels where
        type Rs CreateLabels = CreateLabelsResponse
        request = putJSON workDocs
        response
          = receiveEmpty
              (\ s h x ->
                 CreateLabelsResponse' <$> (pure (fromEnum s)))

instance Hashable CreateLabels where

instance NFData CreateLabels where

instance ToHeaders CreateLabels where
        toHeaders CreateLabels'{..}
          = mconcat
              ["Authentication" =# _clAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateLabels where
        toJSON CreateLabels'{..}
          = object (catMaybes [Just ("Labels" .= _clLabels)])

instance ToPath CreateLabels where
        toPath CreateLabels'{..}
          = mconcat
              ["/api/v1/resources/", toBS _clResourceId, "/labels"]

instance ToQuery CreateLabels where
        toQuery = const mempty

-- | /See:/ 'createLabelsResponse' smart constructor.
newtype CreateLabelsResponse = CreateLabelsResponse'
  { _clrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLabelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clrsResponseStatus' - -- | The response status code.
createLabelsResponse
    :: Int -- ^ 'clrsResponseStatus'
    -> CreateLabelsResponse
createLabelsResponse pResponseStatus_ =
  CreateLabelsResponse' {_clrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
clrsResponseStatus :: Lens' CreateLabelsResponse Int
clrsResponseStatus = lens _clrsResponseStatus (\ s a -> s{_clrsResponseStatus = a})

instance NFData CreateLabelsResponse where
