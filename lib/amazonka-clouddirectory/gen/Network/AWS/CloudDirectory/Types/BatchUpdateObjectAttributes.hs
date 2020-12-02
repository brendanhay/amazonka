{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes where

import Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchUpdate@ operation.
--
--
--
-- /See:/ 'batchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
  { _buoaObjectReference ::
      !ObjectReference,
    _buoaAttributeUpdates ::
      ![ObjectAttributeUpdate]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdateObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buoaObjectReference' - Reference that identifies the object.
--
-- * 'buoaAttributeUpdates' - Attributes update structure.
batchUpdateObjectAttributes ::
  -- | 'buoaObjectReference'
  ObjectReference ->
  BatchUpdateObjectAttributes
batchUpdateObjectAttributes pObjectReference_ =
  BatchUpdateObjectAttributes'
    { _buoaObjectReference =
        pObjectReference_,
      _buoaAttributeUpdates = mempty
    }

-- | Reference that identifies the object.
buoaObjectReference :: Lens' BatchUpdateObjectAttributes ObjectReference
buoaObjectReference = lens _buoaObjectReference (\s a -> s {_buoaObjectReference = a})

-- | Attributes update structure.
buoaAttributeUpdates :: Lens' BatchUpdateObjectAttributes [ObjectAttributeUpdate]
buoaAttributeUpdates = lens _buoaAttributeUpdates (\s a -> s {_buoaAttributeUpdates = a}) . _Coerce

instance Hashable BatchUpdateObjectAttributes

instance NFData BatchUpdateObjectAttributes

instance ToJSON BatchUpdateObjectAttributes where
  toJSON BatchUpdateObjectAttributes' {..} =
    object
      ( catMaybes
          [ Just ("ObjectReference" .= _buoaObjectReference),
            Just ("AttributeUpdates" .= _buoaAttributeUpdates)
          ]
      )
