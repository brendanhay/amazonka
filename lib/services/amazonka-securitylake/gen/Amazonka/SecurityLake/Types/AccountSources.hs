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
-- Module      : Amazonka.SecurityLake.Types.AccountSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AccountSources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.LogsStatus
import Amazonka.SecurityLake.Types.OcsfEventClass

-- | Security Lake can collect logs and events from supported Amazon Web
-- Services services and custom sources.
--
-- /See:/ 'newAccountSources' smart constructor.
data AccountSources = AccountSources'
  { -- | Initializes a new instance of the Event class.
    eventClass :: Prelude.Maybe OcsfEventClass,
    -- | Log status for the Security Lake account.
    logsStatus :: Prelude.Maybe [LogsStatus],
    -- | Account ID of the Security Lake account for which logs are collected.
    account :: Prelude.Text,
    -- | The supported Amazon Web Services services from which logs and events
    -- are collected. Amazon Security Lake supports logs and events collection
    -- for natively-supported Amazon Web Services services. For more
    -- information, see the Amazon Security Lake User Guide.
    sourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventClass', 'accountSources_eventClass' - Initializes a new instance of the Event class.
--
-- 'logsStatus', 'accountSources_logsStatus' - Log status for the Security Lake account.
--
-- 'account', 'accountSources_account' - Account ID of the Security Lake account for which logs are collected.
--
-- 'sourceType', 'accountSources_sourceType' - The supported Amazon Web Services services from which logs and events
-- are collected. Amazon Security Lake supports logs and events collection
-- for natively-supported Amazon Web Services services. For more
-- information, see the Amazon Security Lake User Guide.
newAccountSources ::
  -- | 'account'
  Prelude.Text ->
  -- | 'sourceType'
  Prelude.Text ->
  AccountSources
newAccountSources pAccount_ pSourceType_ =
  AccountSources'
    { eventClass = Prelude.Nothing,
      logsStatus = Prelude.Nothing,
      account = pAccount_,
      sourceType = pSourceType_
    }

-- | Initializes a new instance of the Event class.
accountSources_eventClass :: Lens.Lens' AccountSources (Prelude.Maybe OcsfEventClass)
accountSources_eventClass = Lens.lens (\AccountSources' {eventClass} -> eventClass) (\s@AccountSources' {} a -> s {eventClass = a} :: AccountSources)

-- | Log status for the Security Lake account.
accountSources_logsStatus :: Lens.Lens' AccountSources (Prelude.Maybe [LogsStatus])
accountSources_logsStatus = Lens.lens (\AccountSources' {logsStatus} -> logsStatus) (\s@AccountSources' {} a -> s {logsStatus = a} :: AccountSources) Prelude.. Lens.mapping Lens.coerced

-- | Account ID of the Security Lake account for which logs are collected.
accountSources_account :: Lens.Lens' AccountSources Prelude.Text
accountSources_account = Lens.lens (\AccountSources' {account} -> account) (\s@AccountSources' {} a -> s {account = a} :: AccountSources)

-- | The supported Amazon Web Services services from which logs and events
-- are collected. Amazon Security Lake supports logs and events collection
-- for natively-supported Amazon Web Services services. For more
-- information, see the Amazon Security Lake User Guide.
accountSources_sourceType :: Lens.Lens' AccountSources Prelude.Text
accountSources_sourceType = Lens.lens (\AccountSources' {sourceType} -> sourceType) (\s@AccountSources' {} a -> s {sourceType = a} :: AccountSources)

instance Data.FromJSON AccountSources where
  parseJSON =
    Data.withObject
      "AccountSources"
      ( \x ->
          AccountSources'
            Prelude.<$> (x Data..:? "eventClass")
            Prelude.<*> (x Data..:? "logsStatus" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "account")
            Prelude.<*> (x Data..: "sourceType")
      )

instance Prelude.Hashable AccountSources where
  hashWithSalt _salt AccountSources' {..} =
    _salt `Prelude.hashWithSalt` eventClass
      `Prelude.hashWithSalt` logsStatus
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData AccountSources where
  rnf AccountSources' {..} =
    Prelude.rnf eventClass
      `Prelude.seq` Prelude.rnf logsStatus
      `Prelude.seq` Prelude.rnf account
      `Prelude.seq` Prelude.rnf sourceType
