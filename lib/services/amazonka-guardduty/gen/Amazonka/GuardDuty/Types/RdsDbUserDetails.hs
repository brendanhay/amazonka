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
-- Module      : Amazonka.GuardDuty.Types.RdsDbUserDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RdsDbUserDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the user and authentication details for a
-- database instance involved in the finding.
--
-- /See:/ 'newRdsDbUserDetails' smart constructor.
data RdsDbUserDetails = RdsDbUserDetails'
  { -- | The application name used in the anomalous login attempt.
    application :: Prelude.Maybe Prelude.Text,
    -- | The authentication method used by the user involved in the finding.
    authMethod :: Prelude.Maybe Prelude.Text,
    -- | The name of the database instance involved in the anomalous login
    -- attempt.
    database :: Prelude.Maybe Prelude.Text,
    -- | The version of the Secure Socket Layer (SSL) used for the network.
    ssl :: Prelude.Maybe Prelude.Text,
    -- | The user name used in the anomalous login attempt.
    user :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsDbUserDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'rdsDbUserDetails_application' - The application name used in the anomalous login attempt.
--
-- 'authMethod', 'rdsDbUserDetails_authMethod' - The authentication method used by the user involved in the finding.
--
-- 'database', 'rdsDbUserDetails_database' - The name of the database instance involved in the anomalous login
-- attempt.
--
-- 'ssl', 'rdsDbUserDetails_ssl' - The version of the Secure Socket Layer (SSL) used for the network.
--
-- 'user', 'rdsDbUserDetails_user' - The user name used in the anomalous login attempt.
newRdsDbUserDetails ::
  RdsDbUserDetails
newRdsDbUserDetails =
  RdsDbUserDetails'
    { application = Prelude.Nothing,
      authMethod = Prelude.Nothing,
      database = Prelude.Nothing,
      ssl = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | The application name used in the anomalous login attempt.
rdsDbUserDetails_application :: Lens.Lens' RdsDbUserDetails (Prelude.Maybe Prelude.Text)
rdsDbUserDetails_application = Lens.lens (\RdsDbUserDetails' {application} -> application) (\s@RdsDbUserDetails' {} a -> s {application = a} :: RdsDbUserDetails)

-- | The authentication method used by the user involved in the finding.
rdsDbUserDetails_authMethod :: Lens.Lens' RdsDbUserDetails (Prelude.Maybe Prelude.Text)
rdsDbUserDetails_authMethod = Lens.lens (\RdsDbUserDetails' {authMethod} -> authMethod) (\s@RdsDbUserDetails' {} a -> s {authMethod = a} :: RdsDbUserDetails)

-- | The name of the database instance involved in the anomalous login
-- attempt.
rdsDbUserDetails_database :: Lens.Lens' RdsDbUserDetails (Prelude.Maybe Prelude.Text)
rdsDbUserDetails_database = Lens.lens (\RdsDbUserDetails' {database} -> database) (\s@RdsDbUserDetails' {} a -> s {database = a} :: RdsDbUserDetails)

-- | The version of the Secure Socket Layer (SSL) used for the network.
rdsDbUserDetails_ssl :: Lens.Lens' RdsDbUserDetails (Prelude.Maybe Prelude.Text)
rdsDbUserDetails_ssl = Lens.lens (\RdsDbUserDetails' {ssl} -> ssl) (\s@RdsDbUserDetails' {} a -> s {ssl = a} :: RdsDbUserDetails)

-- | The user name used in the anomalous login attempt.
rdsDbUserDetails_user :: Lens.Lens' RdsDbUserDetails (Prelude.Maybe Prelude.Text)
rdsDbUserDetails_user = Lens.lens (\RdsDbUserDetails' {user} -> user) (\s@RdsDbUserDetails' {} a -> s {user = a} :: RdsDbUserDetails)

instance Data.FromJSON RdsDbUserDetails where
  parseJSON =
    Data.withObject
      "RdsDbUserDetails"
      ( \x ->
          RdsDbUserDetails'
            Prelude.<$> (x Data..:? "application")
            Prelude.<*> (x Data..:? "authMethod")
            Prelude.<*> (x Data..:? "database")
            Prelude.<*> (x Data..:? "ssl")
            Prelude.<*> (x Data..:? "user")
      )

instance Prelude.Hashable RdsDbUserDetails where
  hashWithSalt _salt RdsDbUserDetails' {..} =
    _salt
      `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` authMethod
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` ssl
      `Prelude.hashWithSalt` user

instance Prelude.NFData RdsDbUserDetails where
  rnf RdsDbUserDetails' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf authMethod
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf ssl
      `Prelude.seq` Prelude.rnf user
