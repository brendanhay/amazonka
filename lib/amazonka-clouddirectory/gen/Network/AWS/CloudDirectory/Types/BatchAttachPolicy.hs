{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachPolicy where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attaches a policy object to a regular object inside a 'BatchRead' operation. For more information, see 'AttachPolicy' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchAttachPolicy' smart constructor.
data BatchAttachPolicy = BatchAttachPolicy'
  { _bapPolicyReference ::
      !ObjectReference,
    _bapObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bapPolicyReference' - The reference that is associated with the policy object.
--
-- * 'bapObjectReference' - The reference that identifies the object to which the policy will be attached.
batchAttachPolicy ::
  -- | 'bapPolicyReference'
  ObjectReference ->
  -- | 'bapObjectReference'
  ObjectReference ->
  BatchAttachPolicy
batchAttachPolicy pPolicyReference_ pObjectReference_ =
  BatchAttachPolicy'
    { _bapPolicyReference = pPolicyReference_,
      _bapObjectReference = pObjectReference_
    }

-- | The reference that is associated with the policy object.
bapPolicyReference :: Lens' BatchAttachPolicy ObjectReference
bapPolicyReference = lens _bapPolicyReference (\s a -> s {_bapPolicyReference = a})

-- | The reference that identifies the object to which the policy will be attached.
bapObjectReference :: Lens' BatchAttachPolicy ObjectReference
bapObjectReference = lens _bapObjectReference (\s a -> s {_bapObjectReference = a})

instance Hashable BatchAttachPolicy

instance NFData BatchAttachPolicy

instance ToJSON BatchAttachPolicy where
  toJSON BatchAttachPolicy' {..} =
    object
      ( catMaybes
          [ Just ("PolicyReference" .= _bapPolicyReference),
            Just ("ObjectReference" .= _bapObjectReference)
          ]
      )
