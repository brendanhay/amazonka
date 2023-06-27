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
-- Module      : Amazonka.GuardDuty.Types.RdsLoginAttemptAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RdsLoginAttemptAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.LoginAttribute
import Amazonka.GuardDuty.Types.RemoteIpDetails
import qualified Amazonka.Prelude as Prelude

-- | Indicates that a login attempt was made to the potentially compromised
-- database from a remote IP address.
--
-- /See:/ 'newRdsLoginAttemptAction' smart constructor.
data RdsLoginAttemptAction = RdsLoginAttemptAction'
  { -- | Indicates the login attributes used in the login attempt.
    loginAttributes :: Prelude.Maybe [LoginAttribute],
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsLoginAttemptAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loginAttributes', 'rdsLoginAttemptAction_loginAttributes' - Indicates the login attributes used in the login attempt.
--
-- 'remoteIpDetails', 'rdsLoginAttemptAction_remoteIpDetails' - Undocumented member.
newRdsLoginAttemptAction ::
  RdsLoginAttemptAction
newRdsLoginAttemptAction =
  RdsLoginAttemptAction'
    { loginAttributes =
        Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing
    }

-- | Indicates the login attributes used in the login attempt.
rdsLoginAttemptAction_loginAttributes :: Lens.Lens' RdsLoginAttemptAction (Prelude.Maybe [LoginAttribute])
rdsLoginAttemptAction_loginAttributes = Lens.lens (\RdsLoginAttemptAction' {loginAttributes} -> loginAttributes) (\s@RdsLoginAttemptAction' {} a -> s {loginAttributes = a} :: RdsLoginAttemptAction) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
rdsLoginAttemptAction_remoteIpDetails :: Lens.Lens' RdsLoginAttemptAction (Prelude.Maybe RemoteIpDetails)
rdsLoginAttemptAction_remoteIpDetails = Lens.lens (\RdsLoginAttemptAction' {remoteIpDetails} -> remoteIpDetails) (\s@RdsLoginAttemptAction' {} a -> s {remoteIpDetails = a} :: RdsLoginAttemptAction)

instance Data.FromJSON RdsLoginAttemptAction where
  parseJSON =
    Data.withObject
      "RdsLoginAttemptAction"
      ( \x ->
          RdsLoginAttemptAction'
            Prelude.<$> ( x
                            Data..:? "LoginAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "remoteIpDetails")
      )

instance Prelude.Hashable RdsLoginAttemptAction where
  hashWithSalt _salt RdsLoginAttemptAction' {..} =
    _salt
      `Prelude.hashWithSalt` loginAttributes
      `Prelude.hashWithSalt` remoteIpDetails

instance Prelude.NFData RdsLoginAttemptAction where
  rnf RdsLoginAttemptAction' {..} =
    Prelude.rnf loginAttributes
      `Prelude.seq` Prelude.rnf remoteIpDetails
