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
-- Module      : Amazonka.EFS.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.LifecyclePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EFS.Types.TransitionToIARules
import Amazonka.EFS.Types.TransitionToPrimaryStorageClassRules
import qualified Amazonka.Prelude as Prelude

-- | Describes a policy used by EFS lifecycle management and EFS
-- Intelligent-Tiering that specifies when to transition files into and out
-- of the file system\'s Infrequent Access (IA) storage class. For more
-- information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/lifecycle-management-efs.html EFS Intelligent‐Tiering and EFS Lifecycle Management>.
--
-- When using the @put-lifecycle-configuration@ CLI command or the
-- @PutLifecycleConfiguration@ API action, Amazon EFS requires that each
-- @LifecyclePolicy@ object have only a single transition. This means that
-- in a request body, @LifecyclePolicies@ must be structured as an array of
-- @LifecyclePolicy@ objects, one object for each transition,
-- @TransitionToIA@, @TransitionToPrimaryStorageClass@. For more
-- information, see the request examples in PutLifecycleConfiguration.
--
-- /See:/ 'newLifecyclePolicy' smart constructor.
data LifecyclePolicy = LifecyclePolicy'
  { -- | Describes the period of time that a file is not accessed, after which it
    -- transitions to IA storage. Metadata operations such as listing the
    -- contents of a directory don\'t count as file access events.
    transitionToIA :: Prelude.Maybe TransitionToIARules,
    -- | Describes when to transition a file from IA storage to primary storage.
    -- Metadata operations such as listing the contents of a directory don\'t
    -- count as file access events.
    transitionToPrimaryStorageClass :: Prelude.Maybe TransitionToPrimaryStorageClassRules
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitionToIA', 'lifecyclePolicy_transitionToIA' - Describes the period of time that a file is not accessed, after which it
-- transitions to IA storage. Metadata operations such as listing the
-- contents of a directory don\'t count as file access events.
--
-- 'transitionToPrimaryStorageClass', 'lifecyclePolicy_transitionToPrimaryStorageClass' - Describes when to transition a file from IA storage to primary storage.
-- Metadata operations such as listing the contents of a directory don\'t
-- count as file access events.
newLifecyclePolicy ::
  LifecyclePolicy
newLifecyclePolicy =
  LifecyclePolicy'
    { transitionToIA = Prelude.Nothing,
      transitionToPrimaryStorageClass = Prelude.Nothing
    }

-- | Describes the period of time that a file is not accessed, after which it
-- transitions to IA storage. Metadata operations such as listing the
-- contents of a directory don\'t count as file access events.
lifecyclePolicy_transitionToIA :: Lens.Lens' LifecyclePolicy (Prelude.Maybe TransitionToIARules)
lifecyclePolicy_transitionToIA = Lens.lens (\LifecyclePolicy' {transitionToIA} -> transitionToIA) (\s@LifecyclePolicy' {} a -> s {transitionToIA = a} :: LifecyclePolicy)

-- | Describes when to transition a file from IA storage to primary storage.
-- Metadata operations such as listing the contents of a directory don\'t
-- count as file access events.
lifecyclePolicy_transitionToPrimaryStorageClass :: Lens.Lens' LifecyclePolicy (Prelude.Maybe TransitionToPrimaryStorageClassRules)
lifecyclePolicy_transitionToPrimaryStorageClass = Lens.lens (\LifecyclePolicy' {transitionToPrimaryStorageClass} -> transitionToPrimaryStorageClass) (\s@LifecyclePolicy' {} a -> s {transitionToPrimaryStorageClass = a} :: LifecyclePolicy)

instance Core.FromJSON LifecyclePolicy where
  parseJSON =
    Core.withObject
      "LifecyclePolicy"
      ( \x ->
          LifecyclePolicy'
            Prelude.<$> (x Core..:? "TransitionToIA")
            Prelude.<*> (x Core..:? "TransitionToPrimaryStorageClass")
      )

instance Prelude.Hashable LifecyclePolicy where
  hashWithSalt _salt LifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` transitionToIA
      `Prelude.hashWithSalt` transitionToPrimaryStorageClass

instance Prelude.NFData LifecyclePolicy where
  rnf LifecyclePolicy' {..} =
    Prelude.rnf transitionToIA
      `Prelude.seq` Prelude.rnf transitionToPrimaryStorageClass

instance Core.ToJSON LifecyclePolicy where
  toJSON LifecyclePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TransitionToIA" Core..=)
              Prelude.<$> transitionToIA,
            ("TransitionToPrimaryStorageClass" Core..=)
              Prelude.<$> transitionToPrimaryStorageClass
          ]
      )
