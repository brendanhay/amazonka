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
-- Module      : Amazonka.Neptune.Types.ServerlessV2ScalingConfigurationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.ServerlessV2ScalingConfigurationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Shows the scaling configuration for a Neptune Serverless DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/neptune/latest/userguide/neptune-serverless-using.html Using Amazon Neptune Serverless>
-- in the /Amazon Neptune User Guide/.
--
-- /See:/ 'newServerlessV2ScalingConfigurationInfo' smart constructor.
data ServerlessV2ScalingConfigurationInfo = ServerlessV2ScalingConfigurationInfo'
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
-- Create a value of 'ServerlessV2ScalingConfigurationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacity', 'serverlessV2ScalingConfigurationInfo_maxCapacity' - The maximum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 40, 40.5, 41, and so on.
--
-- 'minCapacity', 'serverlessV2ScalingConfigurationInfo_minCapacity' - The minimum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 8, 8.5, 9, and so on.
newServerlessV2ScalingConfigurationInfo ::
  ServerlessV2ScalingConfigurationInfo
newServerlessV2ScalingConfigurationInfo =
  ServerlessV2ScalingConfigurationInfo'
    { maxCapacity =
        Prelude.Nothing,
      minCapacity = Prelude.Nothing
    }

-- | The maximum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 40, 40.5, 41, and so on.
serverlessV2ScalingConfigurationInfo_maxCapacity :: Lens.Lens' ServerlessV2ScalingConfigurationInfo (Prelude.Maybe Prelude.Double)
serverlessV2ScalingConfigurationInfo_maxCapacity = Lens.lens (\ServerlessV2ScalingConfigurationInfo' {maxCapacity} -> maxCapacity) (\s@ServerlessV2ScalingConfigurationInfo' {} a -> s {maxCapacity = a} :: ServerlessV2ScalingConfigurationInfo)

-- | The minimum number of Neptune capacity units (NCUs) for a DB instance in
-- a Neptune Serverless cluster. You can specify NCU values in half-step
-- increments, such as 8, 8.5, 9, and so on.
serverlessV2ScalingConfigurationInfo_minCapacity :: Lens.Lens' ServerlessV2ScalingConfigurationInfo (Prelude.Maybe Prelude.Double)
serverlessV2ScalingConfigurationInfo_minCapacity = Lens.lens (\ServerlessV2ScalingConfigurationInfo' {minCapacity} -> minCapacity) (\s@ServerlessV2ScalingConfigurationInfo' {} a -> s {minCapacity = a} :: ServerlessV2ScalingConfigurationInfo)

instance
  Data.FromXML
    ServerlessV2ScalingConfigurationInfo
  where
  parseXML x =
    ServerlessV2ScalingConfigurationInfo'
      Prelude.<$> (x Data..@? "MaxCapacity")
      Prelude.<*> (x Data..@? "MinCapacity")

instance
  Prelude.Hashable
    ServerlessV2ScalingConfigurationInfo
  where
  hashWithSalt
    _salt
    ServerlessV2ScalingConfigurationInfo' {..} =
      _salt `Prelude.hashWithSalt` maxCapacity
        `Prelude.hashWithSalt` minCapacity

instance
  Prelude.NFData
    ServerlessV2ScalingConfigurationInfo
  where
  rnf ServerlessV2ScalingConfigurationInfo' {..} =
    Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf minCapacity
