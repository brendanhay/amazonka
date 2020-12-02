{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachToIndex where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attaches the specified object to the specified index inside a 'BatchRead' operation. For more information, see 'AttachToIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchAttachToIndex' smart constructor.
data BatchAttachToIndex = BatchAttachToIndex'
  { _batiIndexReference ::
      !ObjectReference,
    _batiTargetReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachToIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batiIndexReference' - A reference to the index that you are attaching the object to.
--
-- * 'batiTargetReference' - A reference to the object that you are attaching to the index.
batchAttachToIndex ::
  -- | 'batiIndexReference'
  ObjectReference ->
  -- | 'batiTargetReference'
  ObjectReference ->
  BatchAttachToIndex
batchAttachToIndex pIndexReference_ pTargetReference_ =
  BatchAttachToIndex'
    { _batiIndexReference = pIndexReference_,
      _batiTargetReference = pTargetReference_
    }

-- | A reference to the index that you are attaching the object to.
batiIndexReference :: Lens' BatchAttachToIndex ObjectReference
batiIndexReference = lens _batiIndexReference (\s a -> s {_batiIndexReference = a})

-- | A reference to the object that you are attaching to the index.
batiTargetReference :: Lens' BatchAttachToIndex ObjectReference
batiTargetReference = lens _batiTargetReference (\s a -> s {_batiTargetReference = a})

instance Hashable BatchAttachToIndex

instance NFData BatchAttachToIndex

instance ToJSON BatchAttachToIndex where
  toJSON BatchAttachToIndex' {..} =
    object
      ( catMaybes
          [ Just ("IndexReference" .= _batiIndexReference),
            Just ("TargetReference" .= _batiTargetReference)
          ]
      )
