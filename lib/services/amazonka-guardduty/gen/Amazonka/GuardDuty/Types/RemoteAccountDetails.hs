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
-- Module      : Amazonka.GuardDuty.Types.RemoteAccountDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RemoteAccountDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the remote Amazon Web Services account that made
-- the API call.
--
-- /See:/ 'newRemoteAccountDetails' smart constructor.
data RemoteAccountDetails = RemoteAccountDetails'
  { -- | The Amazon Web Services account ID of the remote API caller.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Details on whether the Amazon Web Services account of the remote API
    -- caller is related to your GuardDuty environment. If this value is @True@
    -- the API caller is affiliated to your account in some way. If it is
    -- @False@ the API caller is from outside your environment.
    affiliated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoteAccountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'remoteAccountDetails_accountId' - The Amazon Web Services account ID of the remote API caller.
--
-- 'affiliated', 'remoteAccountDetails_affiliated' - Details on whether the Amazon Web Services account of the remote API
-- caller is related to your GuardDuty environment. If this value is @True@
-- the API caller is affiliated to your account in some way. If it is
-- @False@ the API caller is from outside your environment.
newRemoteAccountDetails ::
  RemoteAccountDetails
newRemoteAccountDetails =
  RemoteAccountDetails'
    { accountId = Prelude.Nothing,
      affiliated = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the remote API caller.
remoteAccountDetails_accountId :: Lens.Lens' RemoteAccountDetails (Prelude.Maybe Prelude.Text)
remoteAccountDetails_accountId = Lens.lens (\RemoteAccountDetails' {accountId} -> accountId) (\s@RemoteAccountDetails' {} a -> s {accountId = a} :: RemoteAccountDetails)

-- | Details on whether the Amazon Web Services account of the remote API
-- caller is related to your GuardDuty environment. If this value is @True@
-- the API caller is affiliated to your account in some way. If it is
-- @False@ the API caller is from outside your environment.
remoteAccountDetails_affiliated :: Lens.Lens' RemoteAccountDetails (Prelude.Maybe Prelude.Bool)
remoteAccountDetails_affiliated = Lens.lens (\RemoteAccountDetails' {affiliated} -> affiliated) (\s@RemoteAccountDetails' {} a -> s {affiliated = a} :: RemoteAccountDetails)

instance Data.FromJSON RemoteAccountDetails where
  parseJSON =
    Data.withObject
      "RemoteAccountDetails"
      ( \x ->
          RemoteAccountDetails'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "affiliated")
      )

instance Prelude.Hashable RemoteAccountDetails where
  hashWithSalt _salt RemoteAccountDetails' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` affiliated

instance Prelude.NFData RemoteAccountDetails where
  rnf RemoteAccountDetails' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf affiliated
