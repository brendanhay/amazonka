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
-- Module      : Amazonka.SSMSAP.Types.Application
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Application where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.ApplicationStatus
import Amazonka.SSMSAP.Types.ApplicationType

-- |
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { type' :: Prelude.Maybe ApplicationType,
    appRegistryArn :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ApplicationStatus,
    id :: Prelude.Maybe Prelude.Text,
    components :: Prelude.Maybe [Prelude.Text],
    lastUpdated :: Prelude.Maybe Data.POSIX,
    statusMessage :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'application_type' -
--
-- 'appRegistryArn', 'application_appRegistryArn' -
--
-- 'arn', 'application_arn' -
--
-- 'status', 'application_status' -
--
-- 'id', 'application_id' -
--
-- 'components', 'application_components' -
--
-- 'lastUpdated', 'application_lastUpdated' -
--
-- 'statusMessage', 'application_statusMessage' -
newApplication ::
  Application
newApplication =
  Application'
    { type' = Prelude.Nothing,
      appRegistryArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      components = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- |
application_type :: Lens.Lens' Application (Prelude.Maybe ApplicationType)
application_type = Lens.lens (\Application' {type'} -> type') (\s@Application' {} a -> s {type' = a} :: Application)

-- |
application_appRegistryArn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_appRegistryArn = Lens.lens (\Application' {appRegistryArn} -> appRegistryArn) (\s@Application' {} a -> s {appRegistryArn = a} :: Application)

-- |
application_arn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- |
application_status :: Lens.Lens' Application (Prelude.Maybe ApplicationStatus)
application_status = Lens.lens (\Application' {status} -> status) (\s@Application' {} a -> s {status = a} :: Application)

-- |
application_id :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_id = Lens.lens (\Application' {id} -> id) (\s@Application' {} a -> s {id = a} :: Application)

-- |
application_components :: Lens.Lens' Application (Prelude.Maybe [Prelude.Text])
application_components = Lens.lens (\Application' {components} -> components) (\s@Application' {} a -> s {components = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- |
application_lastUpdated :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_lastUpdated = Lens.lens (\Application' {lastUpdated} -> lastUpdated) (\s@Application' {} a -> s {lastUpdated = a} :: Application) Prelude.. Lens.mapping Data._Time

-- |
application_statusMessage :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_statusMessage = Lens.lens (\Application' {statusMessage} -> statusMessage) (\s@Application' {} a -> s {statusMessage = a} :: Application)

instance Data.FromJSON Application where
  parseJSON =
    Data.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "AppRegistryArn")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Components" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable Application where
  hashWithSalt _salt Application' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` appRegistryArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf appRegistryArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf statusMessage
