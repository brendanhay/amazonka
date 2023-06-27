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
-- Module      : Amazonka.VoiceId.Types.Fraudster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Fraudster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains all the information about a fraudster.
--
-- /See:/ 'newFraudster' smart constructor.
data Fraudster = Fraudster'
  { -- | The timestamp of when Voice ID identified the fraudster.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the domain that contains the fraudster.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The service-generated identifier for the fraudster.
    generatedFraudsterId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the watchlists the fraudster is a part of.
    watchlistIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Fraudster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'fraudster_createdAt' - The timestamp of when Voice ID identified the fraudster.
--
-- 'domainId', 'fraudster_domainId' - The identifier of the domain that contains the fraudster.
--
-- 'generatedFraudsterId', 'fraudster_generatedFraudsterId' - The service-generated identifier for the fraudster.
--
-- 'watchlistIds', 'fraudster_watchlistIds' - The identifier of the watchlists the fraudster is a part of.
newFraudster ::
  Fraudster
newFraudster =
  Fraudster'
    { createdAt = Prelude.Nothing,
      domainId = Prelude.Nothing,
      generatedFraudsterId = Prelude.Nothing,
      watchlistIds = Prelude.Nothing
    }

-- | The timestamp of when Voice ID identified the fraudster.
fraudster_createdAt :: Lens.Lens' Fraudster (Prelude.Maybe Prelude.UTCTime)
fraudster_createdAt = Lens.lens (\Fraudster' {createdAt} -> createdAt) (\s@Fraudster' {} a -> s {createdAt = a} :: Fraudster) Prelude.. Lens.mapping Data._Time

-- | The identifier of the domain that contains the fraudster.
fraudster_domainId :: Lens.Lens' Fraudster (Prelude.Maybe Prelude.Text)
fraudster_domainId = Lens.lens (\Fraudster' {domainId} -> domainId) (\s@Fraudster' {} a -> s {domainId = a} :: Fraudster)

-- | The service-generated identifier for the fraudster.
fraudster_generatedFraudsterId :: Lens.Lens' Fraudster (Prelude.Maybe Prelude.Text)
fraudster_generatedFraudsterId = Lens.lens (\Fraudster' {generatedFraudsterId} -> generatedFraudsterId) (\s@Fraudster' {} a -> s {generatedFraudsterId = a} :: Fraudster)

-- | The identifier of the watchlists the fraudster is a part of.
fraudster_watchlistIds :: Lens.Lens' Fraudster (Prelude.Maybe [Prelude.Text])
fraudster_watchlistIds = Lens.lens (\Fraudster' {watchlistIds} -> watchlistIds) (\s@Fraudster' {} a -> s {watchlistIds = a} :: Fraudster) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Fraudster where
  parseJSON =
    Data.withObject
      "Fraudster"
      ( \x ->
          Fraudster'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "GeneratedFraudsterId")
            Prelude.<*> (x Data..:? "WatchlistIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Fraudster where
  hashWithSalt _salt Fraudster' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` generatedFraudsterId
      `Prelude.hashWithSalt` watchlistIds

instance Prelude.NFData Fraudster where
  rnf Fraudster' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf generatedFraudsterId
      `Prelude.seq` Prelude.rnf watchlistIds
