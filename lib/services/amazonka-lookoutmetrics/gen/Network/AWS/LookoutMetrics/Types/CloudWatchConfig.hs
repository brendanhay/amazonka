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
-- Module      : Amazonka.LookoutMetrics.Types.CloudWatchConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.CloudWatchConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon CloudWatch datasource.
--
-- /See:/ 'newCloudWatchConfig' smart constructor.
data CloudWatchConfig = CloudWatchConfig'
  { -- | An IAM role that gives Amazon Lookout for Metrics permission to access
    -- data in Amazon CloudWatch.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'cloudWatchConfig_roleArn' - An IAM role that gives Amazon Lookout for Metrics permission to access
-- data in Amazon CloudWatch.
newCloudWatchConfig ::
  -- | 'roleArn'
  Prelude.Text ->
  CloudWatchConfig
newCloudWatchConfig pRoleArn_ =
  CloudWatchConfig' {roleArn = pRoleArn_}

-- | An IAM role that gives Amazon Lookout for Metrics permission to access
-- data in Amazon CloudWatch.
cloudWatchConfig_roleArn :: Lens.Lens' CloudWatchConfig Prelude.Text
cloudWatchConfig_roleArn = Lens.lens (\CloudWatchConfig' {roleArn} -> roleArn) (\s@CloudWatchConfig' {} a -> s {roleArn = a} :: CloudWatchConfig)

instance Core.FromJSON CloudWatchConfig where
  parseJSON =
    Core.withObject
      "CloudWatchConfig"
      ( \x ->
          CloudWatchConfig' Prelude.<$> (x Core..: "RoleArn")
      )

instance Prelude.Hashable CloudWatchConfig

instance Prelude.NFData CloudWatchConfig

instance Core.ToJSON CloudWatchConfig where
  toJSON CloudWatchConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoleArn" Core..= roleArn)]
      )
