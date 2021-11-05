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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceAdmin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceAdmin where

import Amazonka.ChimeSDKIdentity.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of an @AppInstanceAdmin@.
--
-- /See:/ 'newAppInstanceAdmin' smart constructor.
data AppInstanceAdmin = AppInstanceAdmin'
  { -- | The @AppInstanceAdmin@ data.
    admin :: Prelude.Maybe Identity,
    -- | The ARN of the @AppInstance@ for which the user is an administrator.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which an administrator was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'admin', 'appInstanceAdmin_admin' - The @AppInstanceAdmin@ data.
--
-- 'appInstanceArn', 'appInstanceAdmin_appInstanceArn' - The ARN of the @AppInstance@ for which the user is an administrator.
--
-- 'createdTimestamp', 'appInstanceAdmin_createdTimestamp' - The time at which an administrator was created.
newAppInstanceAdmin ::
  AppInstanceAdmin
newAppInstanceAdmin =
  AppInstanceAdmin'
    { admin = Prelude.Nothing,
      appInstanceArn = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The @AppInstanceAdmin@ data.
appInstanceAdmin_admin :: Lens.Lens' AppInstanceAdmin (Prelude.Maybe Identity)
appInstanceAdmin_admin = Lens.lens (\AppInstanceAdmin' {admin} -> admin) (\s@AppInstanceAdmin' {} a -> s {admin = a} :: AppInstanceAdmin)

-- | The ARN of the @AppInstance@ for which the user is an administrator.
appInstanceAdmin_appInstanceArn :: Lens.Lens' AppInstanceAdmin (Prelude.Maybe Prelude.Text)
appInstanceAdmin_appInstanceArn = Lens.lens (\AppInstanceAdmin' {appInstanceArn} -> appInstanceArn) (\s@AppInstanceAdmin' {} a -> s {appInstanceArn = a} :: AppInstanceAdmin)

-- | The time at which an administrator was created.
appInstanceAdmin_createdTimestamp :: Lens.Lens' AppInstanceAdmin (Prelude.Maybe Prelude.UTCTime)
appInstanceAdmin_createdTimestamp = Lens.lens (\AppInstanceAdmin' {createdTimestamp} -> createdTimestamp) (\s@AppInstanceAdmin' {} a -> s {createdTimestamp = a} :: AppInstanceAdmin) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AppInstanceAdmin where
  parseJSON =
    Core.withObject
      "AppInstanceAdmin"
      ( \x ->
          AppInstanceAdmin'
            Prelude.<$> (x Core..:? "Admin")
            Prelude.<*> (x Core..:? "AppInstanceArn")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
      )

instance Prelude.Hashable AppInstanceAdmin

instance Prelude.NFData AppInstanceAdmin
