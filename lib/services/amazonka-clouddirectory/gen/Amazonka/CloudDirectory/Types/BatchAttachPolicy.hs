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
-- Module      : Amazonka.CloudDirectory.Types.BatchAttachPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchAttachPolicy where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attaches a policy object to a regular object inside a BatchRead
-- operation. For more information, see AttachPolicy and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchAttachPolicy' smart constructor.
data BatchAttachPolicy = BatchAttachPolicy'
  { -- | The reference that is associated with the policy object.
    policyReference :: ObjectReference,
    -- | The reference that identifies the object to which the policy will be
    -- attached.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAttachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyReference', 'batchAttachPolicy_policyReference' - The reference that is associated with the policy object.
--
-- 'objectReference', 'batchAttachPolicy_objectReference' - The reference that identifies the object to which the policy will be
-- attached.
newBatchAttachPolicy ::
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  BatchAttachPolicy
newBatchAttachPolicy
  pPolicyReference_
  pObjectReference_ =
    BatchAttachPolicy'
      { policyReference =
          pPolicyReference_,
        objectReference = pObjectReference_
      }

-- | The reference that is associated with the policy object.
batchAttachPolicy_policyReference :: Lens.Lens' BatchAttachPolicy ObjectReference
batchAttachPolicy_policyReference = Lens.lens (\BatchAttachPolicy' {policyReference} -> policyReference) (\s@BatchAttachPolicy' {} a -> s {policyReference = a} :: BatchAttachPolicy)

-- | The reference that identifies the object to which the policy will be
-- attached.
batchAttachPolicy_objectReference :: Lens.Lens' BatchAttachPolicy ObjectReference
batchAttachPolicy_objectReference = Lens.lens (\BatchAttachPolicy' {objectReference} -> objectReference) (\s@BatchAttachPolicy' {} a -> s {objectReference = a} :: BatchAttachPolicy)

instance Prelude.Hashable BatchAttachPolicy where
  hashWithSalt _salt BatchAttachPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyReference
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchAttachPolicy where
  rnf BatchAttachPolicy' {..} =
    Prelude.rnf policyReference
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchAttachPolicy where
  toJSON BatchAttachPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PolicyReference" Data..= policyReference),
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
