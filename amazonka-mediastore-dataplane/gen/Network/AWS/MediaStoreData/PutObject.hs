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
-- Module      : Network.AWS.MediaStoreData.PutObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an object to the specified path. Object sizes are limited to 10 MB.
--
--
module Network.AWS.MediaStoreData.PutObject
    (
    -- * Creating a Request
      putObject
    , PutObject
    -- * Request Lenses
    , poStorageClass
    , poCacheControl
    , poContentType
    , poPath
    , poBody

    -- * Destructuring the Response
    , putObjectResponse
    , PutObjectResponse
    -- * Response Lenses
    , porsETag
    , porsStorageClass
    , porsContentSHA256
    , porsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types
import Network.AWS.MediaStoreData.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putObject' smart constructor.
data PutObject = PutObject'
  { _poStorageClass :: !(Maybe StorageClass)
  , _poCacheControl :: !(Maybe Text)
  , _poContentType  :: !(Maybe Text)
  , _poPath         :: !Text
  , _poBody         :: !HashedBody
  } deriving (Show, Generic)


-- | Creates a value of 'PutObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poStorageClass' - Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
--
-- * 'poCacheControl' - An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> . Headers with a custom user-defined value are also accepted.
--
-- * 'poContentType' - The content type of the object.
--
-- * 'poPath' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name> For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ . Do not include the container name in this path. If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.  There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore. For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> . The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
--
-- * 'poBody' - The bytes to be stored.
putObject
    :: Text -- ^ 'poPath'
    -> HashedBody -- ^ 'poBody'
    -> PutObject
putObject pPath_ pBody_ =
  PutObject'
    { _poStorageClass = Nothing
    , _poCacheControl = Nothing
    , _poContentType = Nothing
    , _poPath = pPath_
    , _poBody = pBody_
    }


-- | Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
poStorageClass :: Lens' PutObject (Maybe StorageClass)
poStorageClass = lens _poStorageClass (\ s a -> s{_poStorageClass = a})

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> . Headers with a custom user-defined value are also accepted.
poCacheControl :: Lens' PutObject (Maybe Text)
poCacheControl = lens _poCacheControl (\ s a -> s{_poCacheControl = a})

-- | The content type of the object.
poContentType :: Lens' PutObject (Maybe Text)
poContentType = lens _poContentType (\ s a -> s{_poContentType = a})

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name> For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ . Do not include the container name in this path. If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.  There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore. For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> . The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
poPath :: Lens' PutObject Text
poPath = lens _poPath (\ s a -> s{_poPath = a})

-- | The bytes to be stored.
poBody :: Lens' PutObject HashedBody
poBody = lens _poBody (\ s a -> s{_poBody = a})

instance AWSRequest PutObject where
        type Rs PutObject = PutObjectResponse
        request = putBody mediaStoreData
        response
          = receiveJSON
              (\ s h x ->
                 PutObjectResponse' <$>
                   (x .?> "ETag") <*> (x .?> "StorageClass") <*>
                     (x .?> "ContentSHA256")
                     <*> (pure (fromEnum s)))

instance ToBody PutObject where
        toBody = toBody . _poBody

instance ToHeaders PutObject where
        toHeaders PutObject'{..}
          = mconcat
              ["x-amz-storage-class" =# _poStorageClass,
               "Cache-Control" =# _poCacheControl,
               "Content-Type" =# _poContentType]

instance ToPath PutObject where
        toPath PutObject'{..} = mconcat ["/", toBS _poPath]

instance ToQuery PutObject where
        toQuery = const mempty

-- | /See:/ 'putObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { _porsETag           :: !(Maybe Text)
  , _porsStorageClass   :: !(Maybe StorageClass)
  , _porsContentSHA256  :: !(Maybe Text)
  , _porsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'porsETag' - Unique identifier of the object in the container.
--
-- * 'porsStorageClass' - The storage class where the object was persisted. Should be “Temporal”.
--
-- * 'porsContentSHA256' - The SHA256 digest of the object that is persisted.
--
-- * 'porsResponseStatus' - -- | The response status code.
putObjectResponse
    :: Int -- ^ 'porsResponseStatus'
    -> PutObjectResponse
putObjectResponse pResponseStatus_ =
  PutObjectResponse'
    { _porsETag = Nothing
    , _porsStorageClass = Nothing
    , _porsContentSHA256 = Nothing
    , _porsResponseStatus = pResponseStatus_
    }


-- | Unique identifier of the object in the container.
porsETag :: Lens' PutObjectResponse (Maybe Text)
porsETag = lens _porsETag (\ s a -> s{_porsETag = a})

-- | The storage class where the object was persisted. Should be “Temporal”.
porsStorageClass :: Lens' PutObjectResponse (Maybe StorageClass)
porsStorageClass = lens _porsStorageClass (\ s a -> s{_porsStorageClass = a})

-- | The SHA256 digest of the object that is persisted.
porsContentSHA256 :: Lens' PutObjectResponse (Maybe Text)
porsContentSHA256 = lens _porsContentSHA256 (\ s a -> s{_porsContentSHA256 = a})

-- | -- | The response status code.
porsResponseStatus :: Lens' PutObjectResponse Int
porsResponseStatus = lens _porsResponseStatus (\ s a -> s{_porsResponseStatus = a})

instance NFData PutObjectResponse where
