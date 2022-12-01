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
-- Module      : Amazonka.Chime.Types.AppInstanceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AppInstanceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of the data for an @AppInstance@.
--
-- /See:/ 'newAppInstanceSummary' smart constructor.
data AppInstanceSummary = AppInstanceSummary'
  { -- | The name of the @AppInstance@.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The metadata of the @AppInstance@.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The @AppInstance@ ARN.
    appInstanceArn :: Prelude.Maybe Prelude.Text
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
-- 'name', 'appInstanceSummary_name' - The name of the @AppInstance@.
--
-- 'metadata', 'appInstanceSummary_metadata' - The metadata of the @AppInstance@.
--
-- 'appInstanceArn', 'appInstanceSummary_appInstanceArn' - The @AppInstance@ ARN.
newAppInstanceSummary ::
  AppInstanceSummary
newAppInstanceSummary =
  AppInstanceSummary'
    { name = Prelude.Nothing,
      metadata = Prelude.Nothing,
      appInstanceArn = Prelude.Nothing
    }

-- | The name of the @AppInstance@.
appInstanceSummary_name :: Lens.Lens' AppInstanceSummary (Prelude.Maybe Prelude.Text)
appInstanceSummary_name = Lens.lens (\AppInstanceSummary' {name} -> name) (\s@AppInstanceSummary' {} a -> s {name = a} :: AppInstanceSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The metadata of the @AppInstance@.
appInstanceSummary_metadata :: Lens.Lens' AppInstanceSummary (Prelude.Maybe Prelude.Text)
appInstanceSummary_metadata = Lens.lens (\AppInstanceSummary' {metadata} -> metadata) (\s@AppInstanceSummary' {} a -> s {metadata = a} :: AppInstanceSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The @AppInstance@ ARN.
appInstanceSummary_appInstanceArn :: Lens.Lens' AppInstanceSummary (Prelude.Maybe Prelude.Text)
appInstanceSummary_appInstanceArn = Lens.lens (\AppInstanceSummary' {appInstanceArn} -> appInstanceArn) (\s@AppInstanceSummary' {} a -> s {appInstanceArn = a} :: AppInstanceSummary)

instance Core.FromJSON AppInstanceSummary where
  parseJSON =
    Core.withObject
      "AppInstanceSummary"
      ( \x ->
          AppInstanceSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "AppInstanceArn")
      )

instance Prelude.Hashable AppInstanceSummary where
  hashWithSalt _salt AppInstanceSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData AppInstanceSummary where
  rnf AppInstanceSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf appInstanceArn
