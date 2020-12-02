{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeletedObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeletedObject where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Information about the deleted object.
--
--
--
-- /See:/ 'deletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { _dVersionId ::
      !(Maybe ObjectVersionId),
    _dDeleteMarker :: !(Maybe Bool),
    _dDeleteMarkerVersionId :: !(Maybe Text),
    _dKey :: !(Maybe ObjectKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletedObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionId' - The version ID of the deleted object.
--
-- * 'dDeleteMarker' - Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
--
-- * 'dDeleteMarkerVersionId' - The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
--
-- * 'dKey' - The name of the deleted object.
deletedObject ::
  DeletedObject
deletedObject =
  DeletedObject'
    { _dVersionId = Nothing,
      _dDeleteMarker = Nothing,
      _dDeleteMarkerVersionId = Nothing,
      _dKey = Nothing
    }

-- | The version ID of the deleted object.
dVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
dVersionId = lens _dVersionId (\s a -> s {_dVersionId = a})

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
dDeleteMarker :: Lens' DeletedObject (Maybe Bool)
dDeleteMarker = lens _dDeleteMarker (\s a -> s {_dDeleteMarker = a})

-- | The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
dDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
dDeleteMarkerVersionId = lens _dDeleteMarkerVersionId (\s a -> s {_dDeleteMarkerVersionId = a})

-- | The name of the deleted object.
dKey :: Lens' DeletedObject (Maybe ObjectKey)
dKey = lens _dKey (\s a -> s {_dKey = a})

instance FromXML DeletedObject where
  parseXML x =
    DeletedObject'
      <$> (x .@? "VersionId")
      <*> (x .@? "DeleteMarker")
      <*> (x .@? "DeleteMarkerVersionId")
      <*> (x .@? "Key")

instance Hashable DeletedObject

instance NFData DeletedObject
