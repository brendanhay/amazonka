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
-- Copyright   : (c) 2013-2023 Brendan Hay
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

-- | An SAP application registered with AWS Systems Manager for SAP.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | The Amazon Resource Name (ARN) of the Application Registry.
    appRegistryArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The components of the application.
    components :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the application.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the application was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The status of the application.
    status :: Prelude.Maybe ApplicationStatus,
    -- | The status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The type of the application.
    type' :: Prelude.Maybe ApplicationType
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
-- 'appRegistryArn', 'application_appRegistryArn' - The Amazon Resource Name (ARN) of the Application Registry.
--
-- 'arn', 'application_arn' - The Amazon Resource Name (ARN) of the application.
--
-- 'components', 'application_components' - The components of the application.
--
-- 'id', 'application_id' - The ID of the application.
--
-- 'lastUpdated', 'application_lastUpdated' - The time at which the application was last updated.
--
-- 'status', 'application_status' - The status of the application.
--
-- 'statusMessage', 'application_statusMessage' - The status message.
--
-- 'type'', 'application_type' - The type of the application.
newApplication ::
  Application
newApplication =
  Application'
    { appRegistryArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      components = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Application Registry.
application_appRegistryArn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_appRegistryArn = Lens.lens (\Application' {appRegistryArn} -> appRegistryArn) (\s@Application' {} a -> s {appRegistryArn = a} :: Application)

-- | The Amazon Resource Name (ARN) of the application.
application_arn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- | The components of the application.
application_components :: Lens.Lens' Application (Prelude.Maybe [Prelude.Text])
application_components = Lens.lens (\Application' {components} -> components) (\s@Application' {} a -> s {components = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application.
application_id :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_id = Lens.lens (\Application' {id} -> id) (\s@Application' {} a -> s {id = a} :: Application)

-- | The time at which the application was last updated.
application_lastUpdated :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_lastUpdated = Lens.lens (\Application' {lastUpdated} -> lastUpdated) (\s@Application' {} a -> s {lastUpdated = a} :: Application) Prelude.. Lens.mapping Data._Time

-- | The status of the application.
application_status :: Lens.Lens' Application (Prelude.Maybe ApplicationStatus)
application_status = Lens.lens (\Application' {status} -> status) (\s@Application' {} a -> s {status = a} :: Application)

-- | The status message.
application_statusMessage :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_statusMessage = Lens.lens (\Application' {statusMessage} -> statusMessage) (\s@Application' {} a -> s {statusMessage = a} :: Application)

-- | The type of the application.
application_type :: Lens.Lens' Application (Prelude.Maybe ApplicationType)
application_type = Lens.lens (\Application' {type'} -> type') (\s@Application' {} a -> s {type' = a} :: Application)

instance Data.FromJSON Application where
  parseJSON =
    Data.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Data..:? "AppRegistryArn")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Components" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Application where
  hashWithSalt _salt Application' {..} =
    _salt
      `Prelude.hashWithSalt` appRegistryArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf appRegistryArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf type'
