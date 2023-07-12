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
-- Module      : Amazonka.MGN.Types.Application
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.Application where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ApplicationAggregatedStatus
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | Application aggregated status.
    applicationAggregatedStatus :: Prelude.Maybe ApplicationAggregatedStatus,
    -- | Application ID.
    applicationID :: Prelude.Maybe Prelude.Text,
    -- | Application ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Application creation dateTime.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | Application description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Application archival status.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | Application last modified dateTime.
    lastModifiedDateTime :: Prelude.Maybe Prelude.Text,
    -- | Application name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Application tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Application wave ID.
    waveID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationAggregatedStatus', 'application_applicationAggregatedStatus' - Application aggregated status.
--
-- 'applicationID', 'application_applicationID' - Application ID.
--
-- 'arn', 'application_arn' - Application ARN.
--
-- 'creationDateTime', 'application_creationDateTime' - Application creation dateTime.
--
-- 'description', 'application_description' - Application description.
--
-- 'isArchived', 'application_isArchived' - Application archival status.
--
-- 'lastModifiedDateTime', 'application_lastModifiedDateTime' - Application last modified dateTime.
--
-- 'name', 'application_name' - Application name.
--
-- 'tags', 'application_tags' - Application tags.
--
-- 'waveID', 'application_waveID' - Application wave ID.
newApplication ::
  Application
newApplication =
  Application'
    { applicationAggregatedStatus =
        Prelude.Nothing,
      applicationID = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      lastModifiedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      waveID = Prelude.Nothing
    }

-- | Application aggregated status.
application_applicationAggregatedStatus :: Lens.Lens' Application (Prelude.Maybe ApplicationAggregatedStatus)
application_applicationAggregatedStatus = Lens.lens (\Application' {applicationAggregatedStatus} -> applicationAggregatedStatus) (\s@Application' {} a -> s {applicationAggregatedStatus = a} :: Application)

-- | Application ID.
application_applicationID :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_applicationID = Lens.lens (\Application' {applicationID} -> applicationID) (\s@Application' {} a -> s {applicationID = a} :: Application)

-- | Application ARN.
application_arn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- | Application creation dateTime.
application_creationDateTime :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_creationDateTime = Lens.lens (\Application' {creationDateTime} -> creationDateTime) (\s@Application' {} a -> s {creationDateTime = a} :: Application)

-- | Application description.
application_description :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_description = Lens.lens (\Application' {description} -> description) (\s@Application' {} a -> s {description = a} :: Application)

-- | Application archival status.
application_isArchived :: Lens.Lens' Application (Prelude.Maybe Prelude.Bool)
application_isArchived = Lens.lens (\Application' {isArchived} -> isArchived) (\s@Application' {} a -> s {isArchived = a} :: Application)

-- | Application last modified dateTime.
application_lastModifiedDateTime :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_lastModifiedDateTime = Lens.lens (\Application' {lastModifiedDateTime} -> lastModifiedDateTime) (\s@Application' {} a -> s {lastModifiedDateTime = a} :: Application)

-- | Application name.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | Application tags.
application_tags :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_tags = Lens.lens (\Application' {tags} -> tags) (\s@Application' {} a -> s {tags = a} :: Application) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Application wave ID.
application_waveID :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_waveID = Lens.lens (\Application' {waveID} -> waveID) (\s@Application' {} a -> s {waveID = a} :: Application)

instance Data.FromJSON Application where
  parseJSON =
    Data.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Data..:? "applicationAggregatedStatus")
            Prelude.<*> (x Data..:? "applicationID")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isArchived")
            Prelude.<*> (x Data..:? "lastModifiedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "waveID")
      )

instance Prelude.Hashable Application where
  hashWithSalt _salt Application' {..} =
    _salt
      `Prelude.hashWithSalt` applicationAggregatedStatus
      `Prelude.hashWithSalt` applicationID
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` lastModifiedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` waveID

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf applicationAggregatedStatus
      `Prelude.seq` Prelude.rnf applicationID
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf lastModifiedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf waveID
