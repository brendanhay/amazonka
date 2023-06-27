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
-- Module      : Amazonka.IoTFleetWise.Types.SignalCatalogSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.SignalCatalogSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a collection of standardized signals, which can be
-- attributes, branches, sensors, or actuators.
--
-- /See:/ 'newSignalCatalogSummary' smart constructor.
data SignalCatalogSummary = SignalCatalogSummary'
  { -- | The Amazon Resource Name (ARN) of the signal catalog.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the signal catalog was created in seconds since epoch (January
    -- 1, 1970 at midnight UTC time).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time the signal catalog was last updated in seconds since epoch
    -- (January 1, 1970 at midnight UTC time).
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the signal catalog.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalCatalogSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'signalCatalogSummary_arn' - The Amazon Resource Name (ARN) of the signal catalog.
--
-- 'creationTime', 'signalCatalogSummary_creationTime' - The time the signal catalog was created in seconds since epoch (January
-- 1, 1970 at midnight UTC time).
--
-- 'lastModificationTime', 'signalCatalogSummary_lastModificationTime' - The time the signal catalog was last updated in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
--
-- 'name', 'signalCatalogSummary_name' - The name of the signal catalog.
newSignalCatalogSummary ::
  SignalCatalogSummary
newSignalCatalogSummary =
  SignalCatalogSummary'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the signal catalog.
signalCatalogSummary_arn :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.Text)
signalCatalogSummary_arn = Lens.lens (\SignalCatalogSummary' {arn} -> arn) (\s@SignalCatalogSummary' {} a -> s {arn = a} :: SignalCatalogSummary)

-- | The time the signal catalog was created in seconds since epoch (January
-- 1, 1970 at midnight UTC time).
signalCatalogSummary_creationTime :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.UTCTime)
signalCatalogSummary_creationTime = Lens.lens (\SignalCatalogSummary' {creationTime} -> creationTime) (\s@SignalCatalogSummary' {} a -> s {creationTime = a} :: SignalCatalogSummary) Prelude.. Lens.mapping Data._Time

-- | The time the signal catalog was last updated in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
signalCatalogSummary_lastModificationTime :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.UTCTime)
signalCatalogSummary_lastModificationTime = Lens.lens (\SignalCatalogSummary' {lastModificationTime} -> lastModificationTime) (\s@SignalCatalogSummary' {} a -> s {lastModificationTime = a} :: SignalCatalogSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the signal catalog.
signalCatalogSummary_name :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.Text)
signalCatalogSummary_name = Lens.lens (\SignalCatalogSummary' {name} -> name) (\s@SignalCatalogSummary' {} a -> s {name = a} :: SignalCatalogSummary)

instance Data.FromJSON SignalCatalogSummary where
  parseJSON =
    Data.withObject
      "SignalCatalogSummary"
      ( \x ->
          SignalCatalogSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastModificationTime")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable SignalCatalogSummary where
  hashWithSalt _salt SignalCatalogSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData SignalCatalogSummary where
  rnf SignalCatalogSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf name
