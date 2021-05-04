{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachPolicy where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detaches the specified policy from the specified directory inside a
-- BatchWrite operation. For more information, see DetachPolicy and
-- BatchWriteRequest$Operations.
--
-- /See:/ 'newBatchDetachPolicy' smart constructor.
data BatchDetachPolicy = BatchDetachPolicy'
  { -- | Reference that identifies the policy object.
    policyReference :: ObjectReference,
    -- | Reference that identifies the object whose policy object will be
    -- detached.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyReference', 'batchDetachPolicy_policyReference' - Reference that identifies the policy object.
--
-- 'objectReference', 'batchDetachPolicy_objectReference' - Reference that identifies the object whose policy object will be
-- detached.
newBatchDetachPolicy ::
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  BatchDetachPolicy
newBatchDetachPolicy
  pPolicyReference_
  pObjectReference_ =
    BatchDetachPolicy'
      { policyReference =
          pPolicyReference_,
        objectReference = pObjectReference_
      }

-- | Reference that identifies the policy object.
batchDetachPolicy_policyReference :: Lens.Lens' BatchDetachPolicy ObjectReference
batchDetachPolicy_policyReference = Lens.lens (\BatchDetachPolicy' {policyReference} -> policyReference) (\s@BatchDetachPolicy' {} a -> s {policyReference = a} :: BatchDetachPolicy)

-- | Reference that identifies the object whose policy object will be
-- detached.
batchDetachPolicy_objectReference :: Lens.Lens' BatchDetachPolicy ObjectReference
batchDetachPolicy_objectReference = Lens.lens (\BatchDetachPolicy' {objectReference} -> objectReference) (\s@BatchDetachPolicy' {} a -> s {objectReference = a} :: BatchDetachPolicy)

instance Prelude.Hashable BatchDetachPolicy

instance Prelude.NFData BatchDetachPolicy

instance Prelude.ToJSON BatchDetachPolicy where
  toJSON BatchDetachPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PolicyReference" Prelude..= policyReference),
            Prelude.Just
              ("ObjectReference" Prelude..= objectReference)
          ]
      )
