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
-- Module      : Amazonka.OpenSearch.Types.SoftwareUpdateOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.SoftwareUpdateOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for configuring service software updates for a domain.
--
-- /See:/ 'newSoftwareUpdateOptions' smart constructor.
data SoftwareUpdateOptions = SoftwareUpdateOptions'
  { -- | Whether automatic service software updates are enabled for the domain.
    autoSoftwareUpdateEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SoftwareUpdateOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoSoftwareUpdateEnabled', 'softwareUpdateOptions_autoSoftwareUpdateEnabled' - Whether automatic service software updates are enabled for the domain.
newSoftwareUpdateOptions ::
  SoftwareUpdateOptions
newSoftwareUpdateOptions =
  SoftwareUpdateOptions'
    { autoSoftwareUpdateEnabled =
        Prelude.Nothing
    }

-- | Whether automatic service software updates are enabled for the domain.
softwareUpdateOptions_autoSoftwareUpdateEnabled :: Lens.Lens' SoftwareUpdateOptions (Prelude.Maybe Prelude.Bool)
softwareUpdateOptions_autoSoftwareUpdateEnabled = Lens.lens (\SoftwareUpdateOptions' {autoSoftwareUpdateEnabled} -> autoSoftwareUpdateEnabled) (\s@SoftwareUpdateOptions' {} a -> s {autoSoftwareUpdateEnabled = a} :: SoftwareUpdateOptions)

instance Data.FromJSON SoftwareUpdateOptions where
  parseJSON =
    Data.withObject
      "SoftwareUpdateOptions"
      ( \x ->
          SoftwareUpdateOptions'
            Prelude.<$> (x Data..:? "AutoSoftwareUpdateEnabled")
      )

instance Prelude.Hashable SoftwareUpdateOptions where
  hashWithSalt _salt SoftwareUpdateOptions' {..} =
    _salt
      `Prelude.hashWithSalt` autoSoftwareUpdateEnabled

instance Prelude.NFData SoftwareUpdateOptions where
  rnf SoftwareUpdateOptions' {..} =
    Prelude.rnf autoSoftwareUpdateEnabled

instance Data.ToJSON SoftwareUpdateOptions where
  toJSON SoftwareUpdateOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoSoftwareUpdateEnabled" Data..=)
              Prelude.<$> autoSoftwareUpdateEnabled
          ]
      )
