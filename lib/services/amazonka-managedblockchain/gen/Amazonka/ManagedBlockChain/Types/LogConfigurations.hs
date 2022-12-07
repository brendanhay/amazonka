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
-- Module      : Amazonka.ManagedBlockChain.Types.LogConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.LogConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.LogConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A collection of log configurations.
--
-- /See:/ 'newLogConfigurations' smart constructor.
data LogConfigurations = LogConfigurations'
  { -- | Parameters for publishing logs to Amazon CloudWatch Logs.
    cloudwatch :: Prelude.Maybe LogConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudwatch', 'logConfigurations_cloudwatch' - Parameters for publishing logs to Amazon CloudWatch Logs.
newLogConfigurations ::
  LogConfigurations
newLogConfigurations =
  LogConfigurations' {cloudwatch = Prelude.Nothing}

-- | Parameters for publishing logs to Amazon CloudWatch Logs.
logConfigurations_cloudwatch :: Lens.Lens' LogConfigurations (Prelude.Maybe LogConfiguration)
logConfigurations_cloudwatch = Lens.lens (\LogConfigurations' {cloudwatch} -> cloudwatch) (\s@LogConfigurations' {} a -> s {cloudwatch = a} :: LogConfigurations)

instance Data.FromJSON LogConfigurations where
  parseJSON =
    Data.withObject
      "LogConfigurations"
      ( \x ->
          LogConfigurations'
            Prelude.<$> (x Data..:? "Cloudwatch")
      )

instance Prelude.Hashable LogConfigurations where
  hashWithSalt _salt LogConfigurations' {..} =
    _salt `Prelude.hashWithSalt` cloudwatch

instance Prelude.NFData LogConfigurations where
  rnf LogConfigurations' {..} = Prelude.rnf cloudwatch

instance Data.ToJSON LogConfigurations where
  toJSON LogConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Cloudwatch" Data..=) Prelude.<$> cloudwatch]
      )
