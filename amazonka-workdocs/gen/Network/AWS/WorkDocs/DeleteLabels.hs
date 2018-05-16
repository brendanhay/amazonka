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
-- Module      : Network.AWS.WorkDocs.DeleteLabels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified list of labels from a resource.
--
--
module Network.AWS.WorkDocs.DeleteLabels
    (
    -- * Creating a Request
      deleteLabels
    , DeleteLabels
    -- * Request Lenses
    , dlDeleteAll
    , dlAuthenticationToken
    , dlLabels
    , dlResourceId

    -- * Destructuring the Response
    , deleteLabelsResponse
    , DeleteLabelsResponse
    -- * Response Lenses
    , dlrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteLabels' smart constructor.
data DeleteLabels = DeleteLabels'
  { _dlDeleteAll           :: !(Maybe Bool)
  , _dlAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dlLabels              :: !(Maybe [Text])
  , _dlResourceId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlDeleteAll' - Flag to request removal of all labels from the specified resource.
--
-- * 'dlAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dlLabels' - List of labels to delete from the resource.
--
-- * 'dlResourceId' - The ID of the resource.
deleteLabels
    :: Text -- ^ 'dlResourceId'
    -> DeleteLabels
deleteLabels pResourceId_ =
  DeleteLabels'
    { _dlDeleteAll = Nothing
    , _dlAuthenticationToken = Nothing
    , _dlLabels = Nothing
    , _dlResourceId = pResourceId_
    }


-- | Flag to request removal of all labels from the specified resource.
dlDeleteAll :: Lens' DeleteLabels (Maybe Bool)
dlDeleteAll = lens _dlDeleteAll (\ s a -> s{_dlDeleteAll = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dlAuthenticationToken :: Lens' DeleteLabels (Maybe Text)
dlAuthenticationToken = lens _dlAuthenticationToken (\ s a -> s{_dlAuthenticationToken = a}) . mapping _Sensitive

-- | List of labels to delete from the resource.
dlLabels :: Lens' DeleteLabels [Text]
dlLabels = lens _dlLabels (\ s a -> s{_dlLabels = a}) . _Default . _Coerce

-- | The ID of the resource.
dlResourceId :: Lens' DeleteLabels Text
dlResourceId = lens _dlResourceId (\ s a -> s{_dlResourceId = a})

instance AWSRequest DeleteLabels where
        type Rs DeleteLabels = DeleteLabelsResponse
        request = delete workDocs
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLabelsResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteLabels where

instance NFData DeleteLabels where

instance ToHeaders DeleteLabels where
        toHeaders DeleteLabels'{..}
          = mconcat
              ["Authentication" =# _dlAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeleteLabels where
        toPath DeleteLabels'{..}
          = mconcat
              ["/api/v1/resources/", toBS _dlResourceId, "/labels"]

instance ToQuery DeleteLabels where
        toQuery DeleteLabels'{..}
          = mconcat
              ["deleteAll" =: _dlDeleteAll,
               "labels" =:
                 toQuery (toQueryList "member" <$> _dlLabels)]

-- | /See:/ 'deleteLabelsResponse' smart constructor.
newtype DeleteLabelsResponse = DeleteLabelsResponse'
  { _dlrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLabelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsResponseStatus' - -- | The response status code.
deleteLabelsResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DeleteLabelsResponse
deleteLabelsResponse pResponseStatus_ =
  DeleteLabelsResponse' {_dlrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlrsResponseStatus :: Lens' DeleteLabelsResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DeleteLabelsResponse where
