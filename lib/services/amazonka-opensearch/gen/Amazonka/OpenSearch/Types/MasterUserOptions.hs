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
-- Module      : Amazonka.OpenSearch.Types.MasterUserOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.MasterUserOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Credentials for the master user for a domain.
--
-- /See:/ 'newMasterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { -- | Amazon Resource Name (ARN) for the master user. Only specify if
    -- @InternalUserDatabaseEnabled@ is @false@.
    masterUserARN :: Prelude.Maybe Prelude.Text,
    -- | User name for the master user. Only specify if
    -- @InternalUserDatabaseEnabled@ is @true@.
    masterUserName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Password for the master user. Only specify if
    -- @InternalUserDatabaseEnabled@ is @true@.
    masterUserPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MasterUserOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterUserARN', 'masterUserOptions_masterUserARN' - Amazon Resource Name (ARN) for the master user. Only specify if
-- @InternalUserDatabaseEnabled@ is @false@.
--
-- 'masterUserName', 'masterUserOptions_masterUserName' - User name for the master user. Only specify if
-- @InternalUserDatabaseEnabled@ is @true@.
--
-- 'masterUserPassword', 'masterUserOptions_masterUserPassword' - Password for the master user. Only specify if
-- @InternalUserDatabaseEnabled@ is @true@.
newMasterUserOptions ::
  MasterUserOptions
newMasterUserOptions =
  MasterUserOptions'
    { masterUserARN = Prelude.Nothing,
      masterUserName = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) for the master user. Only specify if
-- @InternalUserDatabaseEnabled@ is @false@.
masterUserOptions_masterUserARN :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserARN = Lens.lens (\MasterUserOptions' {masterUserARN} -> masterUserARN) (\s@MasterUserOptions' {} a -> s {masterUserARN = a} :: MasterUserOptions)

-- | User name for the master user. Only specify if
-- @InternalUserDatabaseEnabled@ is @true@.
masterUserOptions_masterUserName :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserName = Lens.lens (\MasterUserOptions' {masterUserName} -> masterUserName) (\s@MasterUserOptions' {} a -> s {masterUserName = a} :: MasterUserOptions) Prelude.. Lens.mapping Core._Sensitive

-- | Password for the master user. Only specify if
-- @InternalUserDatabaseEnabled@ is @true@.
masterUserOptions_masterUserPassword :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserPassword = Lens.lens (\MasterUserOptions' {masterUserPassword} -> masterUserPassword) (\s@MasterUserOptions' {} a -> s {masterUserPassword = a} :: MasterUserOptions) Prelude.. Lens.mapping Core._Sensitive

instance Prelude.Hashable MasterUserOptions where
  hashWithSalt _salt MasterUserOptions' {..} =
    _salt `Prelude.hashWithSalt` masterUserARN
      `Prelude.hashWithSalt` masterUserName
      `Prelude.hashWithSalt` masterUserPassword

instance Prelude.NFData MasterUserOptions where
  rnf MasterUserOptions' {..} =
    Prelude.rnf masterUserARN
      `Prelude.seq` Prelude.rnf masterUserName
      `Prelude.seq` Prelude.rnf masterUserPassword

instance Core.ToJSON MasterUserOptions where
  toJSON MasterUserOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MasterUserARN" Core..=) Prelude.<$> masterUserARN,
            ("MasterUserName" Core..=)
              Prelude.<$> masterUserName,
            ("MasterUserPassword" Core..=)
              Prelude.<$> masterUserPassword
          ]
      )
