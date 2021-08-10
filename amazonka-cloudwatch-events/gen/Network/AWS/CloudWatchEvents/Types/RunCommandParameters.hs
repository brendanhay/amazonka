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
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RunCommandParameters where

import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This parameter contains the criteria (either InstanceIds or a tag) used
-- to specify which EC2 instances are to be sent the command.
--
-- /See:/ 'newRunCommandParameters' smart constructor.
data RunCommandParameters = RunCommandParameters'
  { -- | Currently, we support including only one RunCommandTarget block, which
    -- specifies either an array of InstanceIds or a tag.
    runCommandTargets :: Prelude.NonEmpty RunCommandTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunCommandParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runCommandTargets', 'runCommandParameters_runCommandTargets' - Currently, we support including only one RunCommandTarget block, which
-- specifies either an array of InstanceIds or a tag.
newRunCommandParameters ::
  -- | 'runCommandTargets'
  Prelude.NonEmpty RunCommandTarget ->
  RunCommandParameters
newRunCommandParameters pRunCommandTargets_ =
  RunCommandParameters'
    { runCommandTargets =
        Lens._Coerce Lens.# pRunCommandTargets_
    }

-- | Currently, we support including only one RunCommandTarget block, which
-- specifies either an array of InstanceIds or a tag.
runCommandParameters_runCommandTargets :: Lens.Lens' RunCommandParameters (Prelude.NonEmpty RunCommandTarget)
runCommandParameters_runCommandTargets = Lens.lens (\RunCommandParameters' {runCommandTargets} -> runCommandTargets) (\s@RunCommandParameters' {} a -> s {runCommandTargets = a} :: RunCommandParameters) Prelude.. Lens._Coerce

instance Core.FromJSON RunCommandParameters where
  parseJSON =
    Core.withObject
      "RunCommandParameters"
      ( \x ->
          RunCommandParameters'
            Prelude.<$> (x Core..: "RunCommandTargets")
      )

instance Prelude.Hashable RunCommandParameters

instance Prelude.NFData RunCommandParameters

instance Core.ToJSON RunCommandParameters where
  toJSON RunCommandParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RunCommandTargets" Core..= runCommandTargets)
          ]
      )
