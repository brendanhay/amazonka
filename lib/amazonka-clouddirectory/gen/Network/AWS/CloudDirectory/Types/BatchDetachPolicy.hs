{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachPolicy where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detaches the specified policy from the specified directory inside a 'BatchWrite' operation. For more information, see 'DetachPolicy' and 'BatchWriteRequest$Operations' .
--
--
--
-- /See:/ 'batchDetachPolicy' smart constructor.
data BatchDetachPolicy = BatchDetachPolicy'
  { _bdpPolicyReference ::
      !ObjectReference,
    _bdpObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdpPolicyReference' - Reference that identifies the policy object.
--
-- * 'bdpObjectReference' - Reference that identifies the object whose policy object will be detached.
batchDetachPolicy ::
  -- | 'bdpPolicyReference'
  ObjectReference ->
  -- | 'bdpObjectReference'
  ObjectReference ->
  BatchDetachPolicy
batchDetachPolicy pPolicyReference_ pObjectReference_ =
  BatchDetachPolicy'
    { _bdpPolicyReference = pPolicyReference_,
      _bdpObjectReference = pObjectReference_
    }

-- | Reference that identifies the policy object.
bdpPolicyReference :: Lens' BatchDetachPolicy ObjectReference
bdpPolicyReference = lens _bdpPolicyReference (\s a -> s {_bdpPolicyReference = a})

-- | Reference that identifies the object whose policy object will be detached.
bdpObjectReference :: Lens' BatchDetachPolicy ObjectReference
bdpObjectReference = lens _bdpObjectReference (\s a -> s {_bdpObjectReference = a})

instance Hashable BatchDetachPolicy

instance NFData BatchDetachPolicy

instance ToJSON BatchDetachPolicy where
  toJSON BatchDetachPolicy' {..} =
    object
      ( catMaybes
          [ Just ("PolicyReference" .= _bdpPolicyReference),
            Just ("ObjectReference" .= _bdpObjectReference)
          ]
      )
