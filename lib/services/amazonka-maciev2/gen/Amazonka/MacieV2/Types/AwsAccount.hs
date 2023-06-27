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
-- Module      : Amazonka.MacieV2.Types.AwsAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AwsAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Amazon Web Services account and entity
-- that performed an action on an affected resource. The action was
-- performed using the credentials for an Amazon Web Services account other
-- than your own account.
--
-- /See:/ 'newAwsAccount' smart constructor.
data AwsAccount = AwsAccount'
  { -- | The unique identifier for the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the entity that performed the action.
    principalId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'awsAccount_accountId' - The unique identifier for the Amazon Web Services account.
--
-- 'principalId', 'awsAccount_principalId' - The unique identifier for the entity that performed the action.
newAwsAccount ::
  AwsAccount
newAwsAccount =
  AwsAccount'
    { accountId = Prelude.Nothing,
      principalId = Prelude.Nothing
    }

-- | The unique identifier for the Amazon Web Services account.
awsAccount_accountId :: Lens.Lens' AwsAccount (Prelude.Maybe Prelude.Text)
awsAccount_accountId = Lens.lens (\AwsAccount' {accountId} -> accountId) (\s@AwsAccount' {} a -> s {accountId = a} :: AwsAccount)

-- | The unique identifier for the entity that performed the action.
awsAccount_principalId :: Lens.Lens' AwsAccount (Prelude.Maybe Prelude.Text)
awsAccount_principalId = Lens.lens (\AwsAccount' {principalId} -> principalId) (\s@AwsAccount' {} a -> s {principalId = a} :: AwsAccount)

instance Data.FromJSON AwsAccount where
  parseJSON =
    Data.withObject
      "AwsAccount"
      ( \x ->
          AwsAccount'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "principalId")
      )

instance Prelude.Hashable AwsAccount where
  hashWithSalt _salt AwsAccount' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData AwsAccount where
  rnf AwsAccount' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf principalId
