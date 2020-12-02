{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeleteMarkerEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerEntry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Owner

-- | Information about the delete marker.
--
--
--
-- /See:/ 'deleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { _dmeVersionId ::
      !(Maybe ObjectVersionId),
    _dmeIsLatest :: !(Maybe Bool),
    _dmeOwner :: !(Maybe Owner),
    _dmeKey :: !(Maybe ObjectKey),
    _dmeLastModified :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMarkerEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmeVersionId' - Version ID of an object.
--
-- * 'dmeIsLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- * 'dmeOwner' - The account that created the delete marker.>
--
-- * 'dmeKey' - The object key.
--
-- * 'dmeLastModified' - Date and time the object was last modified.
deleteMarkerEntry ::
  DeleteMarkerEntry
deleteMarkerEntry =
  DeleteMarkerEntry'
    { _dmeVersionId = Nothing,
      _dmeIsLatest = Nothing,
      _dmeOwner = Nothing,
      _dmeKey = Nothing,
      _dmeLastModified = Nothing
    }

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\s a -> s {_dmeVersionId = a})

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\s a -> s {_dmeIsLatest = a})

-- | The account that created the delete marker.>
dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\s a -> s {_dmeOwner = a})

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\s a -> s {_dmeKey = a})

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe UTCTime)
dmeLastModified = lens _dmeLastModified (\s a -> s {_dmeLastModified = a}) . mapping _Time

instance FromXML DeleteMarkerEntry where
  parseXML x =
    DeleteMarkerEntry'
      <$> (x .@? "VersionId")
      <*> (x .@? "IsLatest")
      <*> (x .@? "Owner")
      <*> (x .@? "Key")
      <*> (x .@? "LastModified")

instance Hashable DeleteMarkerEntry

instance NFData DeleteMarkerEntry
