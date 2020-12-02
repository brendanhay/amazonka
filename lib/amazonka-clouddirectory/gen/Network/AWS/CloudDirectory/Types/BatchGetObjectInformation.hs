{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformation where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Retrieves metadata about an object inside a 'BatchRead' operation. For more information, see 'GetObjectInformation' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchGetObjectInformation' smart constructor.
newtype BatchGetObjectInformation = BatchGetObjectInformation'
  { _bgoiObjectReference ::
      ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetObjectInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoiObjectReference' - A reference to the object.
batchGetObjectInformation ::
  -- | 'bgoiObjectReference'
  ObjectReference ->
  BatchGetObjectInformation
batchGetObjectInformation pObjectReference_ =
  BatchGetObjectInformation'
    { _bgoiObjectReference =
        pObjectReference_
    }

-- | A reference to the object.
bgoiObjectReference :: Lens' BatchGetObjectInformation ObjectReference
bgoiObjectReference = lens _bgoiObjectReference (\s a -> s {_bgoiObjectReference = a})

instance Hashable BatchGetObjectInformation

instance NFData BatchGetObjectInformation

instance ToJSON BatchGetObjectInformation where
  toJSON BatchGetObjectInformation' {..} =
    object
      (catMaybes [Just ("ObjectReference" .= _bgoiObjectReference)])
