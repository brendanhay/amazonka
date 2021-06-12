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
-- Module      : Network.AWS.Config.Types.RetentionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RetentionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object with the name of the retention configuration and the retention
-- period in days. The object stores the configuration for data retention
-- in AWS Config.
--
-- /See:/ 'newRetentionConfiguration' smart constructor.
data RetentionConfiguration = RetentionConfiguration'
  { -- | The name of the retention configuration object.
    name :: Core.Text,
    -- | Number of days AWS Config stores your historical information.
    --
    -- Currently, only applicable to the configuration item history.
    retentionPeriodInDays :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'retentionPeriodInDays', 'retentionConfiguration_retentionPeriodInDays' - Number of days AWS Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
newRetentionConfiguration ::
  -- | 'name'
  Core.Text ->
  -- | 'retentionPeriodInDays'
  Core.Natural ->
  RetentionConfiguration
newRetentionConfiguration
  pName_
  pRetentionPeriodInDays_ =
    RetentionConfiguration'
      { name = pName_,
        retentionPeriodInDays = pRetentionPeriodInDays_
      }

-- | The name of the retention configuration object.
retentionConfiguration_name :: Lens.Lens' RetentionConfiguration Core.Text
retentionConfiguration_name = Lens.lens (\RetentionConfiguration' {name} -> name) (\s@RetentionConfiguration' {} a -> s {name = a} :: RetentionConfiguration)

-- | Number of days AWS Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
retentionConfiguration_retentionPeriodInDays :: Lens.Lens' RetentionConfiguration Core.Natural
retentionConfiguration_retentionPeriodInDays = Lens.lens (\RetentionConfiguration' {retentionPeriodInDays} -> retentionPeriodInDays) (\s@RetentionConfiguration' {} a -> s {retentionPeriodInDays = a} :: RetentionConfiguration)

instance Core.FromJSON RetentionConfiguration where
  parseJSON =
    Core.withObject
      "RetentionConfiguration"
      ( \x ->
          RetentionConfiguration'
            Core.<$> (x Core..: "Name")
            Core.<*> (x Core..: "RetentionPeriodInDays")
      )

instance Core.Hashable RetentionConfiguration

instance Core.NFData RetentionConfiguration
