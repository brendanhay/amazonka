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
-- Module      : Amazonka.DirectoryService.Types.LogSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.LogSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a log subscription, which tracks real-time data from a chosen
-- log group to a specified destination.
--
-- /See:/ 'newLogSubscription' smart constructor.
data LogSubscription = LogSubscription'
  { -- | Identifier (ID) of the directory that you want to associate with the log
    -- subscription.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the log subscription was created.
    subscriptionCreatedDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'logSubscription_directoryId' - Identifier (ID) of the directory that you want to associate with the log
-- subscription.
--
-- 'logGroupName', 'logSubscription_logGroupName' - The name of the log group.
--
-- 'subscriptionCreatedDateTime', 'logSubscription_subscriptionCreatedDateTime' - The date and time that the log subscription was created.
newLogSubscription ::
  LogSubscription
newLogSubscription =
  LogSubscription'
    { directoryId = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      subscriptionCreatedDateTime = Prelude.Nothing
    }

-- | Identifier (ID) of the directory that you want to associate with the log
-- subscription.
logSubscription_directoryId :: Lens.Lens' LogSubscription (Prelude.Maybe Prelude.Text)
logSubscription_directoryId = Lens.lens (\LogSubscription' {directoryId} -> directoryId) (\s@LogSubscription' {} a -> s {directoryId = a} :: LogSubscription)

-- | The name of the log group.
logSubscription_logGroupName :: Lens.Lens' LogSubscription (Prelude.Maybe Prelude.Text)
logSubscription_logGroupName = Lens.lens (\LogSubscription' {logGroupName} -> logGroupName) (\s@LogSubscription' {} a -> s {logGroupName = a} :: LogSubscription)

-- | The date and time that the log subscription was created.
logSubscription_subscriptionCreatedDateTime :: Lens.Lens' LogSubscription (Prelude.Maybe Prelude.UTCTime)
logSubscription_subscriptionCreatedDateTime = Lens.lens (\LogSubscription' {subscriptionCreatedDateTime} -> subscriptionCreatedDateTime) (\s@LogSubscription' {} a -> s {subscriptionCreatedDateTime = a} :: LogSubscription) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LogSubscription where
  parseJSON =
    Data.withObject
      "LogSubscription"
      ( \x ->
          LogSubscription'
            Prelude.<$> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "SubscriptionCreatedDateTime")
      )

instance Prelude.Hashable LogSubscription where
  hashWithSalt _salt LogSubscription' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` subscriptionCreatedDateTime

instance Prelude.NFData LogSubscription where
  rnf LogSubscription' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf subscriptionCreatedDateTime
