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
-- Module      : Amazonka.IoTFleetWise.Types.TimestreamConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.TimestreamConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Timestream table where the Amazon Web Services IoT FleetWise
-- campaign sends data. Timestream stores and organizes data to optimize
-- query processing time and to reduce storage costs. For more information,
-- see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/data-modeling.html Data modeling>
-- in the /Amazon Timestream Developer Guide/.
--
-- /See:/ 'newTimestreamConfig' smart constructor.
data TimestreamConfig = TimestreamConfig'
  { -- | The Amazon Resource Name (ARN) of the Amazon Timestream table.
    timestreamTableArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task execution role that grants
    -- Amazon Web Services IoT FleetWise permission to deliver data to the
    -- Amazon Timestream table.
    executionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestreamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestreamTableArn', 'timestreamConfig_timestreamTableArn' - The Amazon Resource Name (ARN) of the Amazon Timestream table.
--
-- 'executionRoleArn', 'timestreamConfig_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution role that grants
-- Amazon Web Services IoT FleetWise permission to deliver data to the
-- Amazon Timestream table.
newTimestreamConfig ::
  -- | 'timestreamTableArn'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  TimestreamConfig
newTimestreamConfig
  pTimestreamTableArn_
  pExecutionRoleArn_ =
    TimestreamConfig'
      { timestreamTableArn =
          pTimestreamTableArn_,
        executionRoleArn = pExecutionRoleArn_
      }

-- | The Amazon Resource Name (ARN) of the Amazon Timestream table.
timestreamConfig_timestreamTableArn :: Lens.Lens' TimestreamConfig Prelude.Text
timestreamConfig_timestreamTableArn = Lens.lens (\TimestreamConfig' {timestreamTableArn} -> timestreamTableArn) (\s@TimestreamConfig' {} a -> s {timestreamTableArn = a} :: TimestreamConfig)

-- | The Amazon Resource Name (ARN) of the task execution role that grants
-- Amazon Web Services IoT FleetWise permission to deliver data to the
-- Amazon Timestream table.
timestreamConfig_executionRoleArn :: Lens.Lens' TimestreamConfig Prelude.Text
timestreamConfig_executionRoleArn = Lens.lens (\TimestreamConfig' {executionRoleArn} -> executionRoleArn) (\s@TimestreamConfig' {} a -> s {executionRoleArn = a} :: TimestreamConfig)

instance Data.FromJSON TimestreamConfig where
  parseJSON =
    Data.withObject
      "TimestreamConfig"
      ( \x ->
          TimestreamConfig'
            Prelude.<$> (x Data..: "timestreamTableArn")
            Prelude.<*> (x Data..: "executionRoleArn")
      )

instance Prelude.Hashable TimestreamConfig where
  hashWithSalt _salt TimestreamConfig' {..} =
    _salt
      `Prelude.hashWithSalt` timestreamTableArn
      `Prelude.hashWithSalt` executionRoleArn

instance Prelude.NFData TimestreamConfig where
  rnf TimestreamConfig' {..} =
    Prelude.rnf timestreamTableArn
      `Prelude.seq` Prelude.rnf executionRoleArn

instance Data.ToJSON TimestreamConfig where
  toJSON TimestreamConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("timestreamTableArn" Data..= timestreamTableArn),
            Prelude.Just
              ("executionRoleArn" Data..= executionRoleArn)
          ]
      )
