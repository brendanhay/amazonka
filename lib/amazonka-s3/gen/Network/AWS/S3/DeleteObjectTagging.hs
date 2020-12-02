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
-- Module      : Network.AWS.S3.DeleteObjectTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the tag-set from an existing object.
module Network.AWS.S3.DeleteObjectTagging
    (
    -- * Creating a Request
      deleteObjectTagging
    , DeleteObjectTagging
    -- * Request Lenses
    , dotVersionId
    , dotBucket
    , dotKey

    -- * Destructuring the Response
    , deleteObjectTaggingResponse
    , DeleteObjectTaggingResponse
    -- * Response Lenses
    , dotrsVersionId
    , dotrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteObjectTagging' smart constructor.
data DeleteObjectTagging = DeleteObjectTagging'
  { _dotVersionId :: !(Maybe ObjectVersionId)
  , _dotBucket    :: !BucketName
  , _dotKey       :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotVersionId' - The versionId of the object that the tag-set will be removed from.
--
-- * 'dotBucket' - Undocumented member.
--
-- * 'dotKey' - Undocumented member.
deleteObjectTagging
    :: BucketName -- ^ 'dotBucket'
    -> ObjectKey -- ^ 'dotKey'
    -> DeleteObjectTagging
deleteObjectTagging pBucket_ pKey_ =
  DeleteObjectTagging'
    {_dotVersionId = Nothing, _dotBucket = pBucket_, _dotKey = pKey_}


-- | The versionId of the object that the tag-set will be removed from.
dotVersionId :: Lens' DeleteObjectTagging (Maybe ObjectVersionId)
dotVersionId = lens _dotVersionId (\ s a -> s{_dotVersionId = a})

-- | Undocumented member.
dotBucket :: Lens' DeleteObjectTagging BucketName
dotBucket = lens _dotBucket (\ s a -> s{_dotBucket = a})

-- | Undocumented member.
dotKey :: Lens' DeleteObjectTagging ObjectKey
dotKey = lens _dotKey (\ s a -> s{_dotKey = a})

instance AWSRequest DeleteObjectTagging where
        type Rs DeleteObjectTagging =
             DeleteObjectTaggingResponse
        request = delete s3
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteObjectTaggingResponse' <$>
                   (h .#? "x-amz-version-id") <*> (pure (fromEnum s)))

instance Hashable DeleteObjectTagging where

instance NFData DeleteObjectTagging where

instance ToHeaders DeleteObjectTagging where
        toHeaders = const mempty

instance ToPath DeleteObjectTagging where
        toPath DeleteObjectTagging'{..}
          = mconcat ["/", toBS _dotBucket, "/", toBS _dotKey]

instance ToQuery DeleteObjectTagging where
        toQuery DeleteObjectTagging'{..}
          = mconcat ["versionId" =: _dotVersionId, "tagging"]

-- | /See:/ 'deleteObjectTaggingResponse' smart constructor.
data DeleteObjectTaggingResponse = DeleteObjectTaggingResponse'
  { _dotrsVersionId      :: !(Maybe ObjectVersionId)
  , _dotrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotrsVersionId' - The versionId of the object the tag-set was removed from.
--
-- * 'dotrsResponseStatus' - -- | The response status code.
deleteObjectTaggingResponse
    :: Int -- ^ 'dotrsResponseStatus'
    -> DeleteObjectTaggingResponse
deleteObjectTaggingResponse pResponseStatus_ =
  DeleteObjectTaggingResponse'
    {_dotrsVersionId = Nothing, _dotrsResponseStatus = pResponseStatus_}


-- | The versionId of the object the tag-set was removed from.
dotrsVersionId :: Lens' DeleteObjectTaggingResponse (Maybe ObjectVersionId)
dotrsVersionId = lens _dotrsVersionId (\ s a -> s{_dotrsVersionId = a})

-- | -- | The response status code.
dotrsResponseStatus :: Lens' DeleteObjectTaggingResponse Int
dotrsResponseStatus = lens _dotrsResponseStatus (\ s a -> s{_dotrsResponseStatus = a})

instance NFData DeleteObjectTaggingResponse where
