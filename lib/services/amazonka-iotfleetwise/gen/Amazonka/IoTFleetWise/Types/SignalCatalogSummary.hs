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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.SignalCatalogSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a collection of standardized signals, which can be
-- attributes, branches, sensors, or actuators.
--
-- /See:/ 'newSignalCatalogSummary' smart constructor.
data SignalCatalogSummary = SignalCatalogSummary'
  { -- | The time the signal catalog was last updated in seconds since epoch
    -- (January 1, 1970 at midnight UTC time).
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the signal catalog.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the signal catalog.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the signal catalog was created in seconds since epoch (January
    -- 1, 1970 at midnight UTC time).
    creationTime :: Prelude.Maybe Core.POSIX
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
-- 'lastModificationTime', 'signalCatalogSummary_lastModificationTime' - The time the signal catalog was last updated in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
--
-- 'name', 'signalCatalogSummary_name' - The name of the signal catalog.
--
-- 'arn', 'signalCatalogSummary_arn' - The Amazon Resource Name (ARN) of the signal catalog.
--
-- 'creationTime', 'signalCatalogSummary_creationTime' - The time the signal catalog was created in seconds since epoch (January
-- 1, 1970 at midnight UTC time).
newSignalCatalogSummary ::
  SignalCatalogSummary
newSignalCatalogSummary =
  SignalCatalogSummary'
    { lastModificationTime =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The time the signal catalog was last updated in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
signalCatalogSummary_lastModificationTime :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.UTCTime)
signalCatalogSummary_lastModificationTime = Lens.lens (\SignalCatalogSummary' {lastModificationTime} -> lastModificationTime) (\s@SignalCatalogSummary' {} a -> s {lastModificationTime = a} :: SignalCatalogSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the signal catalog.
signalCatalogSummary_name :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.Text)
signalCatalogSummary_name = Lens.lens (\SignalCatalogSummary' {name} -> name) (\s@SignalCatalogSummary' {} a -> s {name = a} :: SignalCatalogSummary)

-- | The Amazon Resource Name (ARN) of the signal catalog.
signalCatalogSummary_arn :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.Text)
signalCatalogSummary_arn = Lens.lens (\SignalCatalogSummary' {arn} -> arn) (\s@SignalCatalogSummary' {} a -> s {arn = a} :: SignalCatalogSummary)

-- | The time the signal catalog was created in seconds since epoch (January
-- 1, 1970 at midnight UTC time).
signalCatalogSummary_creationTime :: Lens.Lens' SignalCatalogSummary (Prelude.Maybe Prelude.UTCTime)
signalCatalogSummary_creationTime = Lens.lens (\SignalCatalogSummary' {creationTime} -> creationTime) (\s@SignalCatalogSummary' {} a -> s {creationTime = a} :: SignalCatalogSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SignalCatalogSummary where
  parseJSON =
    Core.withObject
      "SignalCatalogSummary"
      ( \x ->
          SignalCatalogSummary'
            Prelude.<$> (x Core..:? "lastModificationTime")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "creationTime")
      )

instance Prelude.Hashable SignalCatalogSummary where
  hashWithSalt _salt SignalCatalogSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData SignalCatalogSummary where
  rnf SignalCatalogSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
