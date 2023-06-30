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
-- Module      : Amazonka.Neptune.Types.ServerlessV2ScalingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.ServerlessV2ScalingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the scaling configuration of a Neptune Serverless DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/neptune/latest/userguide/neptune-serverless-using.html Using Amazon Neptune Serverless>
-- in the /Amazon Neptune User Guide/.
--
-- /See:/ 'newServerlessV2ScalingConfiguration' smart constructor.
data ServerlessV2ScalingConfiguration = ServerlessV2ScalingConfiguration'
  { -- | The maximum number of Neptune capacity units (NCUs) for a DB instance in
    -- a Neptune Serverless cluster. You can specify NCU values in half-step
    -- increments, such as 40, 40.5, 41, and so on.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The minimum number of Neptune capacity units (NCUs) for a DB instance in
    -- a Neptune Serverless cluster. You can specify NCU values in half-step
    -- increments, such as 8, 8.5, 9, and so on.
    minCapacity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerlessV2ScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacity', 'serverlessV2ScalingConfiguration_maxCapacity' - The maximum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 40, 40.5, 41, and so on.
--
-- 'minCapacity', 'serverlessV2ScalingConfiguration_minCapacity' - The minimum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 8, 8.5, 9, and so on.
newServerlessV2ScalingConfiguration ::
  ServerlessV2ScalingConfiguration
newServerlessV2ScalingConfiguration =
  ServerlessV2ScalingConfiguration'
    { maxCapacity =
        Prelude.Nothing,
      minCapacity = Prelude.Nothing
    }

-- | The maximum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 40, 40.5, 41, and so on.
serverlessV2ScalingConfiguration_maxCapacity :: Lens.Lens' ServerlessV2ScalingConfiguration (Prelude.Maybe Prelude.Double)
serverlessV2ScalingConfiguration_maxCapacity = Lens.lens (\ServerlessV2ScalingConfiguration' {maxCapacity} -> maxCapacity) (\s@ServerlessV2ScalingConfiguration' {} a -> s {maxCapacity = a} :: ServerlessV2ScalingConfiguration)

-- | The minimum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 8, 8.5, 9, and so on.
serverlessV2ScalingConfiguration_minCapacity :: Lens.Lens' ServerlessV2ScalingConfiguration (Prelude.Maybe Prelude.Double)
serverlessV2ScalingConfiguration_minCapacity = Lens.lens (\ServerlessV2ScalingConfiguration' {minCapacity} -> minCapacity) (\s@ServerlessV2ScalingConfiguration' {} a -> s {minCapacity = a} :: ServerlessV2ScalingConfiguration)

instance
  Prelude.Hashable
    ServerlessV2ScalingConfiguration
  where
  hashWithSalt
    _salt
    ServerlessV2ScalingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` maxCapacity
        `Prelude.hashWithSalt` minCapacity

instance
  Prelude.NFData
    ServerlessV2ScalingConfiguration
  where
  rnf ServerlessV2ScalingConfiguration' {..} =
    Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf minCapacity

instance
  Data.ToQuery
    ServerlessV2ScalingConfiguration
  where
  toQuery ServerlessV2ScalingConfiguration' {..} =
    Prelude.mconcat
      [ "MaxCapacity" Data.=: maxCapacity,
        "MinCapacity" Data.=: minCapacity
      ]
