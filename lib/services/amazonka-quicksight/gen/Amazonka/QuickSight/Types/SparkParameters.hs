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
-- Module      : Amazonka.QuickSight.Types.SparkParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SparkParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON SparkParameters where
  parseJSON =
    Data.withObject
      "SparkParameters"
      ( \x ->
          SparkParameters'
            Prelude.<$> (x Data..: "Host") Prelude.<*> (x Data..: "Port")
      )

instance Prelude.Hashable SparkParameters where
  hashWithSalt _salt SparkParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port

instance Prelude.NFData SparkParameters where
  rnf SparkParameters' {..} =
    Prelude.rnf host `Prelude.seq` Prelude.rnf port

instance Data.ToJSON SparkParameters where
  toJSON SparkParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port)
          ]
      )
