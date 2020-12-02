{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectReference where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The reference that identifies an object.
--
--
--
-- /See:/ 'objectReference' smart constructor.
newtype ObjectReference = ObjectReference'
  { _orSelector ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orSelector' - A path selector supports easy selection of an object by the parent/child links leading to it from the directory root. Use the link names from each parent/child link to construct the path. Path selectors start with a slash (/) and link names are separated by slashes. For more information about paths, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects> . You can identify an object in one of the following ways:     * /> ObjectIdentifier/ - An object identifier is an opaque string provided by Amazon Cloud Directory. When creating objects, the system will provide you with the identifier of the created object. An object’s identifier is immutable and no two objects will ever share the same object identifier     * /\/some\/path/ - Identifies the object based on path     * /#SomeBatchReference/ - Identifies the object in a batch call
objectReference ::
  ObjectReference
objectReference = ObjectReference' {_orSelector = Nothing}

-- | A path selector supports easy selection of an object by the parent/child links leading to it from the directory root. Use the link names from each parent/child link to construct the path. Path selectors start with a slash (/) and link names are separated by slashes. For more information about paths, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects> . You can identify an object in one of the following ways:     * /> ObjectIdentifier/ - An object identifier is an opaque string provided by Amazon Cloud Directory. When creating objects, the system will provide you with the identifier of the created object. An object’s identifier is immutable and no two objects will ever share the same object identifier     * /\/some\/path/ - Identifies the object based on path     * /#SomeBatchReference/ - Identifies the object in a batch call
orSelector :: Lens' ObjectReference (Maybe Text)
orSelector = lens _orSelector (\s a -> s {_orSelector = a})

instance FromJSON ObjectReference where
  parseJSON =
    withObject
      "ObjectReference"
      (\x -> ObjectReference' <$> (x .:? "Selector"))

instance Hashable ObjectReference

instance NFData ObjectReference

instance ToJSON ObjectReference where
  toJSON ObjectReference' {..} =
    object (catMaybes [("Selector" .=) <$> _orSelector])
