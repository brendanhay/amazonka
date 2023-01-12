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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the data for an @AppInstance@.
--
-- /See:/ 'newAppInstanceSummary' smart constructor.
data AppInstanceSummary = AppInstanceSummary'
  { -- | The @AppInstance@ ARN.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the @AppInstance@.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the @AppInstance@.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'appInstanceSummary_appInstanceArn' - The @AppInstance@ ARN.
--
-- 'metadata', 'appInstanceSummary_metadata' - The metadata of the @AppInstance@.
--
-- 'name', 'appInstanceSummary_name' - The name of the @AppInstance@.
newAppInstanceSummary ::
  AppInstanceSummary
newAppInstanceSummary =
  AppInstanceSummary'
    { appInstanceArn =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The @AppInstance@ ARN.
appInstanceSummary_appInstanceArn :: Lens.Lens' AppInstanceSummary (Prelude.Maybe Prelude.Text)
appInstanceSummary_appInstanceArn = Lens.lens (\AppInstanceSummary' {appInstanceArn} -> appInstanceArn) (\s@AppInstanceSummary' {} a -> s {appInstanceArn = a} :: AppInstanceSummary)

-- | The metadata of the @AppInstance@.
appInstanceSummary_metadata :: Lens.Lens' AppInstanceSummary (Prelude.Maybe Prelude.Text)
appInstanceSummary_metadata = Lens.lens (\AppInstanceSummary' {metadata} -> metadata) (\s@AppInstanceSummary' {} a -> s {metadata = a} :: AppInstanceSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the @AppInstance@.
appInstanceSummary_name :: Lens.Lens' AppInstanceSummary (Prelude.Maybe Prelude.Text)
appInstanceSummary_name = Lens.lens (\AppInstanceSummary' {name} -> name) (\s@AppInstanceSummary' {} a -> s {name = a} :: AppInstanceSummary) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON AppInstanceSummary where
  parseJSON =
    Data.withObject
      "AppInstanceSummary"
      ( \x ->
          AppInstanceSummary'
            Prelude.<$> (x Data..:? "AppInstanceArn")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AppInstanceSummary where
  hashWithSalt _salt AppInstanceSummary' {..} =
    _salt `Prelude.hashWithSalt` appInstanceArn
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name

instance Prelude.NFData AppInstanceSummary where
  rnf AppInstanceSummary' {..} =
    Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
