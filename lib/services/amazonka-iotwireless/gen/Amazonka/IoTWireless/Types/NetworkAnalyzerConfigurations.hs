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
-- Module      : Amazonka.IoTWireless.Types.NetworkAnalyzerConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.NetworkAnalyzerConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network analyzer configurations.
--
-- /See:/ 'newNetworkAnalyzerConfigurations' smart constructor.
data NetworkAnalyzerConfigurations = NetworkAnalyzerConfigurations'
  { name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name of the new resource.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkAnalyzerConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'networkAnalyzerConfigurations_name' - Undocumented member.
--
-- 'arn', 'networkAnalyzerConfigurations_arn' - The Amazon Resource Name of the new resource.
newNetworkAnalyzerConfigurations ::
  NetworkAnalyzerConfigurations
newNetworkAnalyzerConfigurations =
  NetworkAnalyzerConfigurations'
    { name =
        Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | Undocumented member.
networkAnalyzerConfigurations_name :: Lens.Lens' NetworkAnalyzerConfigurations (Prelude.Maybe Prelude.Text)
networkAnalyzerConfigurations_name = Lens.lens (\NetworkAnalyzerConfigurations' {name} -> name) (\s@NetworkAnalyzerConfigurations' {} a -> s {name = a} :: NetworkAnalyzerConfigurations)

-- | The Amazon Resource Name of the new resource.
networkAnalyzerConfigurations_arn :: Lens.Lens' NetworkAnalyzerConfigurations (Prelude.Maybe Prelude.Text)
networkAnalyzerConfigurations_arn = Lens.lens (\NetworkAnalyzerConfigurations' {arn} -> arn) (\s@NetworkAnalyzerConfigurations' {} a -> s {arn = a} :: NetworkAnalyzerConfigurations)

instance Data.FromJSON NetworkAnalyzerConfigurations where
  parseJSON =
    Data.withObject
      "NetworkAnalyzerConfigurations"
      ( \x ->
          NetworkAnalyzerConfigurations'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Arn")
      )

instance
  Prelude.Hashable
    NetworkAnalyzerConfigurations
  where
  hashWithSalt _salt NetworkAnalyzerConfigurations' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData NetworkAnalyzerConfigurations where
  rnf NetworkAnalyzerConfigurations' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn
