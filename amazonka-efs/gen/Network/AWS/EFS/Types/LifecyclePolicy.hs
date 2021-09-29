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
-- Module      : Network.AWS.EFS.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecyclePolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.TransitionToIARules
import Network.AWS.EFS.Types.TransitionToPrimaryStorageClassRules
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a policy used by EFS lifecycle management to transition files
-- to the Infrequent Access (IA) storage class.
--
-- /See:/ 'newLifecyclePolicy' smart constructor.
data LifecyclePolicy = LifecyclePolicy'
  { -- | Describes the period of time that a file is not accessed, after which it
    -- transitions to the IA storage class. Metadata operations such as listing
    -- the contents of a directory don\'t count as file access events.
    transitionToIA :: Prelude.Maybe TransitionToIARules,
    -- | Describes the policy used to transition a file from infequent access
    -- storage to primary storage.
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
-- transitions to the IA storage class. Metadata operations such as listing
-- the contents of a directory don\'t count as file access events.
--
-- 'transitionToPrimaryStorageClass', 'lifecyclePolicy_transitionToPrimaryStorageClass' - Describes the policy used to transition a file from infequent access
-- storage to primary storage.
newLifecyclePolicy ::
  LifecyclePolicy
newLifecyclePolicy =
  LifecyclePolicy'
    { transitionToIA = Prelude.Nothing,
      transitionToPrimaryStorageClass = Prelude.Nothing
    }

-- | Describes the period of time that a file is not accessed, after which it
-- transitions to the IA storage class. Metadata operations such as listing
-- the contents of a directory don\'t count as file access events.
lifecyclePolicy_transitionToIA :: Lens.Lens' LifecyclePolicy (Prelude.Maybe TransitionToIARules)
lifecyclePolicy_transitionToIA = Lens.lens (\LifecyclePolicy' {transitionToIA} -> transitionToIA) (\s@LifecyclePolicy' {} a -> s {transitionToIA = a} :: LifecyclePolicy)

-- | Describes the policy used to transition a file from infequent access
-- storage to primary storage.
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

instance Prelude.Hashable LifecyclePolicy

instance Prelude.NFData LifecyclePolicy

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
