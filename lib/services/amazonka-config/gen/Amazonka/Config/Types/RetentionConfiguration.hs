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
-- Module      : Amazonka.Config.Types.RetentionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RetentionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object with the name of the retention configuration and the retention
-- period in days. The object stores the configuration for data retention
-- in Config.
--
-- /See:/ 'newRetentionConfiguration' smart constructor.
data RetentionConfiguration = RetentionConfiguration'
  { -- | The name of the retention configuration object.
    name :: Prelude.Text,
    -- | Number of days Config stores your historical information.
    --
    -- Currently, only applicable to the configuration item history.
    retentionPeriodInDays :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'retentionConfiguration_name' - The name of the retention configuration object.
--
-- 'retentionPeriodInDays', 'retentionConfiguration_retentionPeriodInDays' - Number of days Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
newRetentionConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'retentionPeriodInDays'
  Prelude.Natural ->
  RetentionConfiguration
newRetentionConfiguration
  pName_
  pRetentionPeriodInDays_ =
    RetentionConfiguration'
      { name = pName_,
        retentionPeriodInDays = pRetentionPeriodInDays_
      }

-- | The name of the retention configuration object.
retentionConfiguration_name :: Lens.Lens' RetentionConfiguration Prelude.Text
retentionConfiguration_name = Lens.lens (\RetentionConfiguration' {name} -> name) (\s@RetentionConfiguration' {} a -> s {name = a} :: RetentionConfiguration)

-- | Number of days Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
retentionConfiguration_retentionPeriodInDays :: Lens.Lens' RetentionConfiguration Prelude.Natural
retentionConfiguration_retentionPeriodInDays = Lens.lens (\RetentionConfiguration' {retentionPeriodInDays} -> retentionPeriodInDays) (\s@RetentionConfiguration' {} a -> s {retentionPeriodInDays = a} :: RetentionConfiguration)

instance Core.FromJSON RetentionConfiguration where
  parseJSON =
    Core.withObject
      "RetentionConfiguration"
      ( \x ->
          RetentionConfiguration'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "RetentionPeriodInDays")
      )

instance Prelude.Hashable RetentionConfiguration where
  hashWithSalt _salt RetentionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` retentionPeriodInDays

instance Prelude.NFData RetentionConfiguration where
  rnf RetentionConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf retentionPeriodInDays
