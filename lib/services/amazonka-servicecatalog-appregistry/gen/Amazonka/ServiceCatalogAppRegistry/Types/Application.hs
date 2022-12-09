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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.Application
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.Application where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a Amazon Web Services Service Catalog AppRegistry application
-- that is the top-level node in a hierarchy of related cloud resource
-- abstractions.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | The Amazon resource name (ARN) that specifies the application across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the application.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the application. The name must be unique in the region in
    -- which you are creating the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs you can use to associate with the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'application_arn' - The Amazon resource name (ARN) that specifies the application across
-- services.
--
-- 'creationTime', 'application_creationTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- created.
--
-- 'description', 'application_description' - The description of the application.
--
-- 'id', 'application_id' - The identifier of the application.
--
-- 'lastUpdateTime', 'application_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
--
-- 'name', 'application_name' - The name of the application. The name must be unique in the region in
-- which you are creating the application.
--
-- 'tags', 'application_tags' - Key-value pairs you can use to associate with the application.
newApplication ::
  Application
newApplication =
  Application'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) that specifies the application across
-- services.
application_arn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- created.
application_creationTime :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_creationTime = Lens.lens (\Application' {creationTime} -> creationTime) (\s@Application' {} a -> s {creationTime = a} :: Application) Prelude.. Lens.mapping Data._Time

-- | The description of the application.
application_description :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_description = Lens.lens (\Application' {description} -> description) (\s@Application' {} a -> s {description = a} :: Application)

-- | The identifier of the application.
application_id :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_id = Lens.lens (\Application' {id} -> id) (\s@Application' {} a -> s {id = a} :: Application)

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
application_lastUpdateTime :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_lastUpdateTime = Lens.lens (\Application' {lastUpdateTime} -> lastUpdateTime) (\s@Application' {} a -> s {lastUpdateTime = a} :: Application) Prelude.. Lens.mapping Data._Time

-- | The name of the application. The name must be unique in the region in
-- which you are creating the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | Key-value pairs you can use to associate with the application.
application_tags :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_tags = Lens.lens (\Application' {tags} -> tags) (\s@Application' {} a -> s {tags = a} :: Application) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Application where
  parseJSON =
    Data.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Application where
  hashWithSalt _salt Application' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
