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
-- Module      : Network.AWS.QuickSight.Types.SparkParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.SparkParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The parameters for Spark.
--
-- /See:/ 'newSparkParameters' smart constructor.
data SparkParameters = SparkParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SparkParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'sparkParameters_host' - Host.
--
-- 'port', 'sparkParameters_port' - Port.
newSparkParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  SparkParameters
newSparkParameters pHost_ pPort_ =
  SparkParameters' {host = pHost_, port = pPort_}

-- | Host.
sparkParameters_host :: Lens.Lens' SparkParameters Prelude.Text
sparkParameters_host = Lens.lens (\SparkParameters' {host} -> host) (\s@SparkParameters' {} a -> s {host = a} :: SparkParameters)

-- | Port.
sparkParameters_port :: Lens.Lens' SparkParameters Prelude.Natural
sparkParameters_port = Lens.lens (\SparkParameters' {port} -> port) (\s@SparkParameters' {} a -> s {port = a} :: SparkParameters)

instance Core.FromJSON SparkParameters where
  parseJSON =
    Core.withObject
      "SparkParameters"
      ( \x ->
          SparkParameters'
            Prelude.<$> (x Core..: "Host") Prelude.<*> (x Core..: "Port")
      )

instance Prelude.Hashable SparkParameters

instance Prelude.NFData SparkParameters

instance Core.ToJSON SparkParameters where
  toJSON SparkParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Core..= host),
            Prelude.Just ("Port" Core..= port)
          ]
      )
