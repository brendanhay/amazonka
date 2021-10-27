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
-- Module      : Network.AWS.ServiceCatalogAppRegistry.Types.Application
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalogAppRegistry.Types.Application where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a Amazon Web Services Service Catalog AppRegistry application
-- that is the top-level node in a hierarchy of related cloud resource
-- abstractions.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon resource name (ARN) that specifies the application across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the application. The name must be unique in the region in
    -- which you are creating the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the application.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- last updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'creationTime', 'application_creationTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- created.
--
-- 'arn', 'application_arn' - The Amazon resource name (ARN) that specifies the application across
-- services.
--
-- 'name', 'application_name' - The name of the application. The name must be unique in the region in
-- which you are creating the application.
--
-- 'id', 'application_id' - The identifier of the application.
--
-- 'lastUpdateTime', 'application_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
--
-- 'description', 'application_description' - The description of the application.
--
-- 'tags', 'application_tags' - Key-value pairs you can use to associate with the application.
newApplication ::
  Application
newApplication =
  Application'
    { creationTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- created.
application_creationTime :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_creationTime = Lens.lens (\Application' {creationTime} -> creationTime) (\s@Application' {} a -> s {creationTime = a} :: Application) Prelude.. Lens.mapping Core._Time

-- | The Amazon resource name (ARN) that specifies the application across
-- services.
application_arn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- | The name of the application. The name must be unique in the region in
-- which you are creating the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | The identifier of the application.
application_id :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_id = Lens.lens (\Application' {id} -> id) (\s@Application' {} a -> s {id = a} :: Application)

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
application_lastUpdateTime :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_lastUpdateTime = Lens.lens (\Application' {lastUpdateTime} -> lastUpdateTime) (\s@Application' {} a -> s {lastUpdateTime = a} :: Application) Prelude.. Lens.mapping Core._Time

-- | The description of the application.
application_description :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_description = Lens.lens (\Application' {description} -> description) (\s@Application' {} a -> s {description = a} :: Application)

-- | Key-value pairs you can use to associate with the application.
application_tags :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_tags = Lens.lens (\Application' {tags} -> tags) (\s@Application' {} a -> s {tags = a} :: Application) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Application where
  parseJSON =
    Core.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Application

instance Prelude.NFData Application
