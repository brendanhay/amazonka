{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Object
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Object where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectStorageClass
import Network.AWS.S3.Types.Owner

-- | An object consists of data and its descriptive metadata.
--
--
--
-- /See:/ 'object'' smart constructor.
data Object = Object'
  { _oOwner :: !(Maybe Owner),
    _oETag :: !ETag,
    _oSize :: !Int,
    _oKey :: !ObjectKey,
    _oStorageClass :: !ObjectStorageClass,
    _oLastModified :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Object' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOwner' - The owner of the object
--
-- * 'oETag' - The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
--
-- * 'oSize' - Size in bytes of the object
--
-- * 'oKey' - The name that you assign to an object. You use the object key to retrieve the object.
--
-- * 'oStorageClass' - The class of storage used to store the object.
--
-- * 'oLastModified' - The date the Object was Last Modified
object' ::
  -- | 'oETag'
  ETag ->
  -- | 'oSize'
  Int ->
  -- | 'oKey'
  ObjectKey ->
  -- | 'oStorageClass'
  ObjectStorageClass ->
  -- | 'oLastModified'
  UTCTime ->
  Object
object' pETag_ pSize_ pKey_ pStorageClass_ pLastModified_ =
  Object'
    { _oOwner = Nothing,
      _oETag = pETag_,
      _oSize = pSize_,
      _oKey = pKey_,
      _oStorageClass = pStorageClass_,
      _oLastModified = _Time # pLastModified_
    }

-- | The owner of the object
oOwner :: Lens' Object (Maybe Owner)
oOwner = lens _oOwner (\s a -> s {_oOwner = a})

-- | The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
oETag :: Lens' Object ETag
oETag = lens _oETag (\s a -> s {_oETag = a})

-- | Size in bytes of the object
oSize :: Lens' Object Int
oSize = lens _oSize (\s a -> s {_oSize = a})

-- | The name that you assign to an object. You use the object key to retrieve the object.
oKey :: Lens' Object ObjectKey
oKey = lens _oKey (\s a -> s {_oKey = a})

-- | The class of storage used to store the object.
oStorageClass :: Lens' Object ObjectStorageClass
oStorageClass = lens _oStorageClass (\s a -> s {_oStorageClass = a})

-- | The date the Object was Last Modified
oLastModified :: Lens' Object UTCTime
oLastModified = lens _oLastModified (\s a -> s {_oLastModified = a}) . _Time

instance FromXML Object where
  parseXML x =
    Object'
      <$> (x .@? "Owner")
      <*> (x .@ "ETag")
      <*> (x .@ "Size")
      <*> (x .@ "Key")
      <*> (x .@ "StorageClass")
      <*> (x .@ "LastModified")

instance Hashable Object

instance NFData Object
