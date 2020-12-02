{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDeleteObject where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'DeleteObject' operation.
--
--
--
-- /See:/ 'batchDeleteObject' smart constructor.
newtype BatchDeleteObject = BatchDeleteObject'
  { _bdoObjectReference ::
      ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoObjectReference' - The reference that identifies the object.
batchDeleteObject ::
  -- | 'bdoObjectReference'
  ObjectReference ->
  BatchDeleteObject
batchDeleteObject pObjectReference_ =
  BatchDeleteObject' {_bdoObjectReference = pObjectReference_}

-- | The reference that identifies the object.
bdoObjectReference :: Lens' BatchDeleteObject ObjectReference
bdoObjectReference = lens _bdoObjectReference (\s a -> s {_bdoObjectReference = a})

instance Hashable BatchDeleteObject

instance NFData BatchDeleteObject

instance ToJSON BatchDeleteObject where
  toJSON BatchDeleteObject' {..} =
    object
      (catMaybes [Just ("ObjectReference" .= _bdoObjectReference)])
