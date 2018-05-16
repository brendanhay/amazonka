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
-- Module      : Network.AWS.SageMaker.DeleteTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from an Amazon SageMaker resource.
--
--
-- To list a resource's tags, use the @ListTags@ API.
--
module Network.AWS.SageMaker.DeleteTags
    (
    -- * Creating a Request
      deleteTags
    , DeleteTags
    -- * Request Lenses
    , dtResourceARN
    , dtTagKeys

    -- * Destructuring the Response
    , deleteTagsResponse
    , DeleteTagsResponse
    -- * Response Lenses
    , dtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { _dtResourceARN :: !Text
  , _dtTagKeys     :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtResourceARN' - The Amazon Resource Name (ARN) of the resource whose tags you want to delete.
--
-- * 'dtTagKeys' - An array or one or more tag keys to delete.
deleteTags
    :: Text -- ^ 'dtResourceARN'
    -> NonEmpty Text -- ^ 'dtTagKeys'
    -> DeleteTags
deleteTags pResourceARN_ pTagKeys_ =
  DeleteTags' {_dtResourceARN = pResourceARN_, _dtTagKeys = _List1 # pTagKeys_}


-- | The Amazon Resource Name (ARN) of the resource whose tags you want to delete.
dtResourceARN :: Lens' DeleteTags Text
dtResourceARN = lens _dtResourceARN (\ s a -> s{_dtResourceARN = a})

-- | An array or one or more tag keys to delete.
dtTagKeys :: Lens' DeleteTags (NonEmpty Text)
dtTagKeys = lens _dtTagKeys (\ s a -> s{_dtTagKeys = a}) . _List1

instance AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        request = postJSON sageMaker
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTagsResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTags where

instance NFData DeleteTags where

instance ToHeaders DeleteTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTags where
        toJSON DeleteTags'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _dtResourceARN),
                  Just ("TagKeys" .= _dtTagKeys)])

instance ToPath DeleteTags where
        toPath = const "/"

instance ToQuery DeleteTags where
        toQuery = const mempty

-- | /See:/ 'deleteTagsResponse' smart constructor.
newtype DeleteTagsResponse = DeleteTagsResponse'
  { _dtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsResponseStatus' - -- | The response status code.
deleteTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DeleteTagsResponse
deleteTagsResponse pResponseStatus_ =
  DeleteTagsResponse' {_dtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtrsResponseStatus :: Lens' DeleteTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DeleteTagsResponse where
