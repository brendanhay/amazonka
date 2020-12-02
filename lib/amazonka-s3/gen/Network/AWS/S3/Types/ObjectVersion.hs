{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectVersion where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectVersionStorageClass
import Network.AWS.S3.Types.Owner

-- | The version of an object.
--
--
--
-- /See:/ 'objectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { _ovETag :: !(Maybe ETag),
    _ovVersionId :: !(Maybe ObjectVersionId),
    _ovSize :: !(Maybe Int),
    _ovIsLatest :: !(Maybe Bool),
    _ovOwner :: !(Maybe Owner),
    _ovKey :: !(Maybe ObjectKey),
    _ovStorageClass :: !(Maybe ObjectVersionStorageClass),
    _ovLastModified :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovETag' - The entity tag is an MD5 hash of that version of the object.
--
-- * 'ovVersionId' - Version ID of an object.
--
-- * 'ovSize' - Size in bytes of the object.
--
-- * 'ovIsLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- * 'ovOwner' - Specifies the owner of the object.
--
-- * 'ovKey' - The object key.
--
-- * 'ovStorageClass' - The class of storage used to store the object.
--
-- * 'ovLastModified' - Date and time the object was last modified.
objectVersion ::
  ObjectVersion
objectVersion =
  ObjectVersion'
    { _ovETag = Nothing,
      _ovVersionId = Nothing,
      _ovSize = Nothing,
      _ovIsLatest = Nothing,
      _ovOwner = Nothing,
      _ovKey = Nothing,
      _ovStorageClass = Nothing,
      _ovLastModified = Nothing
    }

-- | The entity tag is an MD5 hash of that version of the object.
ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\s a -> s {_ovETag = a})

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\s a -> s {_ovVersionId = a})

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Int)
ovSize = lens _ovSize (\s a -> s {_ovSize = a})

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\s a -> s {_ovIsLatest = a})

-- | Specifies the owner of the object.
ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\s a -> s {_ovOwner = a})

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe ObjectKey)
ovKey = lens _ovKey (\s a -> s {_ovKey = a})

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
ovStorageClass = lens _ovStorageClass (\s a -> s {_ovStorageClass = a})

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe UTCTime)
ovLastModified = lens _ovLastModified (\s a -> s {_ovLastModified = a}) . mapping _Time

instance FromXML ObjectVersion where
  parseXML x =
    ObjectVersion'
      <$> (x .@? "ETag")
      <*> (x .@? "VersionId")
      <*> (x .@? "Size")
      <*> (x .@? "IsLatest")
      <*> (x .@? "Owner")
      <*> (x .@? "Key")
      <*> (x .@? "StorageClass")
      <*> (x .@? "LastModified")

instance Hashable ObjectVersion

instance NFData ObjectVersion
