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
-- Module      : Network.AWS.S3.GetObjectTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag-set of an object.
module Network.AWS.S3.GetObjectTagging
    (
    -- * Creating a Request
      getObjectTagging
    , GetObjectTagging
    -- * Request Lenses
    , gotoVersionId
    , gotoBucket
    , gotoKey

    -- * Destructuring the Response
    , getObjectTaggingResponse
    , GetObjectTaggingResponse
    -- * Response Lenses
    , gotrsVersionId
    , gotrsResponseStatus
    , gotrsTagSet
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getObjectTagging' smart constructor.
data GetObjectTagging = GetObjectTagging'
  { _gotoVersionId :: !(Maybe ObjectVersionId)
  , _gotoBucket    :: !BucketName
  , _gotoKey       :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotoVersionId' - Undocumented member.
--
-- * 'gotoBucket' - Undocumented member.
--
-- * 'gotoKey' - Undocumented member.
getObjectTagging
    :: BucketName -- ^ 'gotoBucket'
    -> ObjectKey -- ^ 'gotoKey'
    -> GetObjectTagging
getObjectTagging pBucket_ pKey_ =
  GetObjectTagging'
    {_gotoVersionId = Nothing, _gotoBucket = pBucket_, _gotoKey = pKey_}


-- | Undocumented member.
gotoVersionId :: Lens' GetObjectTagging (Maybe ObjectVersionId)
gotoVersionId = lens _gotoVersionId (\ s a -> s{_gotoVersionId = a})

-- | Undocumented member.
gotoBucket :: Lens' GetObjectTagging BucketName
gotoBucket = lens _gotoBucket (\ s a -> s{_gotoBucket = a})

-- | Undocumented member.
gotoKey :: Lens' GetObjectTagging ObjectKey
gotoKey = lens _gotoKey (\ s a -> s{_gotoKey = a})

instance AWSRequest GetObjectTagging where
        type Rs GetObjectTagging = GetObjectTaggingResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetObjectTaggingResponse' <$>
                   (h .#? "x-amz-version-id") <*> (pure (fromEnum s))
                     <*>
                     (x .@? "TagSet" .!@ mempty >>= parseXMLList "Tag"))

instance Hashable GetObjectTagging where

instance NFData GetObjectTagging where

instance ToHeaders GetObjectTagging where
        toHeaders = const mempty

instance ToPath GetObjectTagging where
        toPath GetObjectTagging'{..}
          = mconcat ["/", toBS _gotoBucket, "/", toBS _gotoKey]

instance ToQuery GetObjectTagging where
        toQuery GetObjectTagging'{..}
          = mconcat ["versionId" =: _gotoVersionId, "tagging"]

-- | /See:/ 'getObjectTaggingResponse' smart constructor.
data GetObjectTaggingResponse = GetObjectTaggingResponse'
  { _gotrsVersionId      :: !(Maybe ObjectVersionId)
  , _gotrsResponseStatus :: !Int
  , _gotrsTagSet         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotrsVersionId' - Undocumented member.
--
-- * 'gotrsResponseStatus' - -- | The response status code.
--
-- * 'gotrsTagSet' - Undocumented member.
getObjectTaggingResponse
    :: Int -- ^ 'gotrsResponseStatus'
    -> GetObjectTaggingResponse
getObjectTaggingResponse pResponseStatus_ =
  GetObjectTaggingResponse'
    { _gotrsVersionId = Nothing
    , _gotrsResponseStatus = pResponseStatus_
    , _gotrsTagSet = mempty
    }


-- | Undocumented member.
gotrsVersionId :: Lens' GetObjectTaggingResponse (Maybe ObjectVersionId)
gotrsVersionId = lens _gotrsVersionId (\ s a -> s{_gotrsVersionId = a})

-- | -- | The response status code.
gotrsResponseStatus :: Lens' GetObjectTaggingResponse Int
gotrsResponseStatus = lens _gotrsResponseStatus (\ s a -> s{_gotrsResponseStatus = a})

-- | Undocumented member.
gotrsTagSet :: Lens' GetObjectTaggingResponse [Tag]
gotrsTagSet = lens _gotrsTagSet (\ s a -> s{_gotrsTagSet = a}) . _Coerce

instance NFData GetObjectTaggingResponse where
