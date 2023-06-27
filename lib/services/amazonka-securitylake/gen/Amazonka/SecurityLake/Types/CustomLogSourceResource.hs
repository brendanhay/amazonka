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
-- Module      : Amazonka.SecurityLake.Types.CustomLogSourceResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.CustomLogSourceResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.CustomLogSourceAttributes
import Amazonka.SecurityLake.Types.CustomLogSourceProvider

-- | Amazon Security Lake can collect logs and events from third-party custom
-- sources.
--
-- /See:/ 'newCustomLogSourceResource' smart constructor.
data CustomLogSourceResource = CustomLogSourceResource'
  { -- | The attributes of a third-party custom source.
    attributes :: Prelude.Maybe CustomLogSourceAttributes,
    -- | The details of the log provider for a third-party custom source.
    provider :: Prelude.Maybe CustomLogSourceProvider,
    -- | The name for a third-party custom source. This must be a Regionally
    -- unique value.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The version for a third-party custom source. This must be a Regionally
    -- unique value.
    sourceVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLogSourceResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'customLogSourceResource_attributes' - The attributes of a third-party custom source.
--
-- 'provider', 'customLogSourceResource_provider' - The details of the log provider for a third-party custom source.
--
-- 'sourceName', 'customLogSourceResource_sourceName' - The name for a third-party custom source. This must be a Regionally
-- unique value.
--
-- 'sourceVersion', 'customLogSourceResource_sourceVersion' - The version for a third-party custom source. This must be a Regionally
-- unique value.
newCustomLogSourceResource ::
  CustomLogSourceResource
newCustomLogSourceResource =
  CustomLogSourceResource'
    { attributes =
        Prelude.Nothing,
      provider = Prelude.Nothing,
      sourceName = Prelude.Nothing,
      sourceVersion = Prelude.Nothing
    }

-- | The attributes of a third-party custom source.
customLogSourceResource_attributes :: Lens.Lens' CustomLogSourceResource (Prelude.Maybe CustomLogSourceAttributes)
customLogSourceResource_attributes = Lens.lens (\CustomLogSourceResource' {attributes} -> attributes) (\s@CustomLogSourceResource' {} a -> s {attributes = a} :: CustomLogSourceResource)

-- | The details of the log provider for a third-party custom source.
customLogSourceResource_provider :: Lens.Lens' CustomLogSourceResource (Prelude.Maybe CustomLogSourceProvider)
customLogSourceResource_provider = Lens.lens (\CustomLogSourceResource' {provider} -> provider) (\s@CustomLogSourceResource' {} a -> s {provider = a} :: CustomLogSourceResource)

-- | The name for a third-party custom source. This must be a Regionally
-- unique value.
customLogSourceResource_sourceName :: Lens.Lens' CustomLogSourceResource (Prelude.Maybe Prelude.Text)
customLogSourceResource_sourceName = Lens.lens (\CustomLogSourceResource' {sourceName} -> sourceName) (\s@CustomLogSourceResource' {} a -> s {sourceName = a} :: CustomLogSourceResource)

-- | The version for a third-party custom source. This must be a Regionally
-- unique value.
customLogSourceResource_sourceVersion :: Lens.Lens' CustomLogSourceResource (Prelude.Maybe Prelude.Text)
customLogSourceResource_sourceVersion = Lens.lens (\CustomLogSourceResource' {sourceVersion} -> sourceVersion) (\s@CustomLogSourceResource' {} a -> s {sourceVersion = a} :: CustomLogSourceResource)

instance Data.FromJSON CustomLogSourceResource where
  parseJSON =
    Data.withObject
      "CustomLogSourceResource"
      ( \x ->
          CustomLogSourceResource'
            Prelude.<$> (x Data..:? "attributes")
            Prelude.<*> (x Data..:? "provider")
            Prelude.<*> (x Data..:? "sourceName")
            Prelude.<*> (x Data..:? "sourceVersion")
      )

instance Prelude.Hashable CustomLogSourceResource where
  hashWithSalt _salt CustomLogSourceResource' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` sourceVersion

instance Prelude.NFData CustomLogSourceResource where
  rnf CustomLogSourceResource' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf sourceVersion

instance Data.ToJSON CustomLogSourceResource where
  toJSON CustomLogSourceResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("provider" Data..=) Prelude.<$> provider,
            ("sourceName" Data..=) Prelude.<$> sourceName,
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion
          ]
      )
