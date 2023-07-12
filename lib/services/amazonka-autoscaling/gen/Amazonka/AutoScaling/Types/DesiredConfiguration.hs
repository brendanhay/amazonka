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
-- Module      : Amazonka.AutoScaling.Types.DesiredConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.DesiredConfiguration where

import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import Amazonka.AutoScaling.Types.MixedInstancesPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the desired configuration for an instance refresh.
--
-- If you specify a desired configuration, you must specify either a
-- @LaunchTemplate@ or a @MixedInstancesPolicy@.
--
-- /See:/ 'newDesiredConfiguration' smart constructor.
data DesiredConfiguration = DesiredConfiguration'
  { launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DesiredConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'desiredConfiguration_launchTemplate' - Undocumented member.
--
-- 'mixedInstancesPolicy', 'desiredConfiguration_mixedInstancesPolicy' - Undocumented member.
newDesiredConfiguration ::
  DesiredConfiguration
newDesiredConfiguration =
  DesiredConfiguration'
    { launchTemplate =
        Prelude.Nothing,
      mixedInstancesPolicy = Prelude.Nothing
    }

-- | Undocumented member.
desiredConfiguration_launchTemplate :: Lens.Lens' DesiredConfiguration (Prelude.Maybe LaunchTemplateSpecification)
desiredConfiguration_launchTemplate = Lens.lens (\DesiredConfiguration' {launchTemplate} -> launchTemplate) (\s@DesiredConfiguration' {} a -> s {launchTemplate = a} :: DesiredConfiguration)

-- | Undocumented member.
desiredConfiguration_mixedInstancesPolicy :: Lens.Lens' DesiredConfiguration (Prelude.Maybe MixedInstancesPolicy)
desiredConfiguration_mixedInstancesPolicy = Lens.lens (\DesiredConfiguration' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@DesiredConfiguration' {} a -> s {mixedInstancesPolicy = a} :: DesiredConfiguration)

instance Data.FromXML DesiredConfiguration where
  parseXML x =
    DesiredConfiguration'
      Prelude.<$> (x Data..@? "LaunchTemplate")
      Prelude.<*> (x Data..@? "MixedInstancesPolicy")

instance Prelude.Hashable DesiredConfiguration where
  hashWithSalt _salt DesiredConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` mixedInstancesPolicy

instance Prelude.NFData DesiredConfiguration where
  rnf DesiredConfiguration' {..} =
    Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy

instance Data.ToQuery DesiredConfiguration where
  toQuery DesiredConfiguration' {..} =
    Prelude.mconcat
      [ "LaunchTemplate" Data.=: launchTemplate,
        "MixedInstancesPolicy" Data.=: mixedInstancesPolicy
      ]
