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
-- Module      : Amazonka.Detective.Types.Administrator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.Administrator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Detective administrator account for an
-- organization.
--
-- /See:/ 'newAdministrator' smart constructor.
data Administrator = Administrator'
  { -- | The Amazon Web Services account identifier of the Detective
    -- administrator account for the organization.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the Detective administrator account was enabled.
    -- The value is an ISO8601 formatted string. For example,
    -- @2021-08-18T16:35:56.284Z@.
    delegationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the organization behavior graph.
    graphArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Administrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'administrator_accountId' - The Amazon Web Services account identifier of the Detective
-- administrator account for the organization.
--
-- 'delegationTime', 'administrator_delegationTime' - The date and time when the Detective administrator account was enabled.
-- The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
--
-- 'graphArn', 'administrator_graphArn' - The ARN of the organization behavior graph.
newAdministrator ::
  Administrator
newAdministrator =
  Administrator'
    { accountId = Prelude.Nothing,
      delegationTime = Prelude.Nothing,
      graphArn = Prelude.Nothing
    }

-- | The Amazon Web Services account identifier of the Detective
-- administrator account for the organization.
administrator_accountId :: Lens.Lens' Administrator (Prelude.Maybe Prelude.Text)
administrator_accountId = Lens.lens (\Administrator' {accountId} -> accountId) (\s@Administrator' {} a -> s {accountId = a} :: Administrator)

-- | The date and time when the Detective administrator account was enabled.
-- The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
administrator_delegationTime :: Lens.Lens' Administrator (Prelude.Maybe Prelude.UTCTime)
administrator_delegationTime = Lens.lens (\Administrator' {delegationTime} -> delegationTime) (\s@Administrator' {} a -> s {delegationTime = a} :: Administrator) Prelude.. Lens.mapping Data._Time

-- | The ARN of the organization behavior graph.
administrator_graphArn :: Lens.Lens' Administrator (Prelude.Maybe Prelude.Text)
administrator_graphArn = Lens.lens (\Administrator' {graphArn} -> graphArn) (\s@Administrator' {} a -> s {graphArn = a} :: Administrator)

instance Data.FromJSON Administrator where
  parseJSON =
    Data.withObject
      "Administrator"
      ( \x ->
          Administrator'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "DelegationTime")
            Prelude.<*> (x Data..:? "GraphArn")
      )

instance Prelude.Hashable Administrator where
  hashWithSalt _salt Administrator' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` delegationTime
      `Prelude.hashWithSalt` graphArn

instance Prelude.NFData Administrator where
  rnf Administrator' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf delegationTime
      `Prelude.seq` Prelude.rnf graphArn
