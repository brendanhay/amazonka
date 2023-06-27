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
-- Module      : Amazonka.SecurityLake.Types.LogSourceResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.LogSourceResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsLogSourceResource
import Amazonka.SecurityLake.Types.CustomLogSourceResource

-- | The supported source types from which logs and events are collected in
-- Amazon Security Lake. For the list of supported Amazon Web Services, see
-- the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
--
-- /See:/ 'newLogSourceResource' smart constructor.
data LogSourceResource = LogSourceResource'
  { -- | Amazon Security Lake supports log and event collection for natively
    -- supported Amazon Web Services.
    awsLogSource :: Prelude.Maybe AwsLogSourceResource,
    -- | Amazon Security Lake supports custom source types. For a detailed list,
    -- see the Amazon Security Lake User Guide.
    customLogSource :: Prelude.Maybe CustomLogSourceResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogSourceResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsLogSource', 'logSourceResource_awsLogSource' - Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
--
-- 'customLogSource', 'logSourceResource_customLogSource' - Amazon Security Lake supports custom source types. For a detailed list,
-- see the Amazon Security Lake User Guide.
newLogSourceResource ::
  LogSourceResource
newLogSourceResource =
  LogSourceResource'
    { awsLogSource = Prelude.Nothing,
      customLogSource = Prelude.Nothing
    }

-- | Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
logSourceResource_awsLogSource :: Lens.Lens' LogSourceResource (Prelude.Maybe AwsLogSourceResource)
logSourceResource_awsLogSource = Lens.lens (\LogSourceResource' {awsLogSource} -> awsLogSource) (\s@LogSourceResource' {} a -> s {awsLogSource = a} :: LogSourceResource)

-- | Amazon Security Lake supports custom source types. For a detailed list,
-- see the Amazon Security Lake User Guide.
logSourceResource_customLogSource :: Lens.Lens' LogSourceResource (Prelude.Maybe CustomLogSourceResource)
logSourceResource_customLogSource = Lens.lens (\LogSourceResource' {customLogSource} -> customLogSource) (\s@LogSourceResource' {} a -> s {customLogSource = a} :: LogSourceResource)

instance Data.FromJSON LogSourceResource where
  parseJSON =
    Data.withObject
      "LogSourceResource"
      ( \x ->
          LogSourceResource'
            Prelude.<$> (x Data..:? "awsLogSource")
            Prelude.<*> (x Data..:? "customLogSource")
      )

instance Prelude.Hashable LogSourceResource where
  hashWithSalt _salt LogSourceResource' {..} =
    _salt
      `Prelude.hashWithSalt` awsLogSource
      `Prelude.hashWithSalt` customLogSource

instance Prelude.NFData LogSourceResource where
  rnf LogSourceResource' {..} =
    Prelude.rnf awsLogSource
      `Prelude.seq` Prelude.rnf customLogSource

instance Data.ToJSON LogSourceResource where
  toJSON LogSourceResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsLogSource" Data..=) Prelude.<$> awsLogSource,
            ("customLogSource" Data..=)
              Prelude.<$> customLogSource
          ]
      )
