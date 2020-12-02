{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndex where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detaches the specified object from the specified index inside a 'BatchRead' operation. For more information, see 'DetachFromIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchDetachFromIndex' smart constructor.
data BatchDetachFromIndex = BatchDetachFromIndex'
  { _bdfiIndexReference ::
      !ObjectReference,
    _bdfiTargetReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachFromIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdfiIndexReference' - A reference to the index object.
--
-- * 'bdfiTargetReference' - A reference to the object being detached from the index.
batchDetachFromIndex ::
  -- | 'bdfiIndexReference'
  ObjectReference ->
  -- | 'bdfiTargetReference'
  ObjectReference ->
  BatchDetachFromIndex
batchDetachFromIndex pIndexReference_ pTargetReference_ =
  BatchDetachFromIndex'
    { _bdfiIndexReference = pIndexReference_,
      _bdfiTargetReference = pTargetReference_
    }

-- | A reference to the index object.
bdfiIndexReference :: Lens' BatchDetachFromIndex ObjectReference
bdfiIndexReference = lens _bdfiIndexReference (\s a -> s {_bdfiIndexReference = a})

-- | A reference to the object being detached from the index.
bdfiTargetReference :: Lens' BatchDetachFromIndex ObjectReference
bdfiTargetReference = lens _bdfiTargetReference (\s a -> s {_bdfiTargetReference = a})

instance Hashable BatchDetachFromIndex

instance NFData BatchDetachFromIndex

instance ToJSON BatchDetachFromIndex where
  toJSON BatchDetachFromIndex' {..} =
    object
      ( catMaybes
          [ Just ("IndexReference" .= _bdfiIndexReference),
            Just ("TargetReference" .= _bdfiTargetReference)
          ]
      )
