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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ApplicationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of a Amazon Web Services Service Catalog AppRegistry
-- application.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | The Amazon resource name (ARN) that specifies the application across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the application.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- last updated.
    lastUpdateTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the application. The name must be unique in the region in
    -- which you are creating the application.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'applicationSummary_arn' - The Amazon resource name (ARN) that specifies the application across
-- services.
--
-- 'creationTime', 'applicationSummary_creationTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- created.
--
-- 'description', 'applicationSummary_description' - The description of the application.
--
-- 'id', 'applicationSummary_id' - The identifier of the application.
--
-- 'lastUpdateTime', 'applicationSummary_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
--
-- 'name', 'applicationSummary_name' - The name of the application. The name must be unique in the region in
-- which you are creating the application.
newApplicationSummary ::
  ApplicationSummary
newApplicationSummary =
  ApplicationSummary'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) that specifies the application across
-- services.
applicationSummary_arn :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_arn = Lens.lens (\ApplicationSummary' {arn} -> arn) (\s@ApplicationSummary' {} a -> s {arn = a} :: ApplicationSummary)

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- created.
applicationSummary_creationTime :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.UTCTime)
applicationSummary_creationTime = Lens.lens (\ApplicationSummary' {creationTime} -> creationTime) (\s@ApplicationSummary' {} a -> s {creationTime = a} :: ApplicationSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the application.
applicationSummary_description :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_description = Lens.lens (\ApplicationSummary' {description} -> description) (\s@ApplicationSummary' {} a -> s {description = a} :: ApplicationSummary)

-- | The identifier of the application.
applicationSummary_id :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_id = Lens.lens (\ApplicationSummary' {id} -> id) (\s@ApplicationSummary' {} a -> s {id = a} :: ApplicationSummary)

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
applicationSummary_lastUpdateTime :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.UTCTime)
applicationSummary_lastUpdateTime = Lens.lens (\ApplicationSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ApplicationSummary' {} a -> s {lastUpdateTime = a} :: ApplicationSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the application. The name must be unique in the region in
-- which you are creating the application.
applicationSummary_name :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_name = Lens.lens (\ApplicationSummary' {name} -> name) (\s@ApplicationSummary' {} a -> s {name = a} :: ApplicationSummary)

instance Data.FromJSON ApplicationSummary where
  parseJSON =
    Data.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt _salt ApplicationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf lastUpdateTime `Prelude.seq`
              Prelude.rnf name
