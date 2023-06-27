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
-- Module      : Amazonka.VoiceId.Types.FraudsterSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudsterSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of information about a fraudster.
--
-- /See:/ 'newFraudsterSummary' smart constructor.
data FraudsterSummary = FraudsterSummary'
  { -- | The timestamp of when the fraudster summary was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the domain that contains the fraudster summary.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The service-generated identifier for the fraudster.
    generatedFraudsterId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the watchlists the fraudster is a part of.
    watchlistIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FraudsterSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'fraudsterSummary_createdAt' - The timestamp of when the fraudster summary was created.
--
-- 'domainId', 'fraudsterSummary_domainId' - The identifier of the domain that contains the fraudster summary.
--
-- 'generatedFraudsterId', 'fraudsterSummary_generatedFraudsterId' - The service-generated identifier for the fraudster.
--
-- 'watchlistIds', 'fraudsterSummary_watchlistIds' - The identifier of the watchlists the fraudster is a part of.
newFraudsterSummary ::
  FraudsterSummary
newFraudsterSummary =
  FraudsterSummary'
    { createdAt = Prelude.Nothing,
      domainId = Prelude.Nothing,
      generatedFraudsterId = Prelude.Nothing,
      watchlistIds = Prelude.Nothing
    }

-- | The timestamp of when the fraudster summary was created.
fraudsterSummary_createdAt :: Lens.Lens' FraudsterSummary (Prelude.Maybe Prelude.UTCTime)
fraudsterSummary_createdAt = Lens.lens (\FraudsterSummary' {createdAt} -> createdAt) (\s@FraudsterSummary' {} a -> s {createdAt = a} :: FraudsterSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the domain that contains the fraudster summary.
fraudsterSummary_domainId :: Lens.Lens' FraudsterSummary (Prelude.Maybe Prelude.Text)
fraudsterSummary_domainId = Lens.lens (\FraudsterSummary' {domainId} -> domainId) (\s@FraudsterSummary' {} a -> s {domainId = a} :: FraudsterSummary)

-- | The service-generated identifier for the fraudster.
fraudsterSummary_generatedFraudsterId :: Lens.Lens' FraudsterSummary (Prelude.Maybe Prelude.Text)
fraudsterSummary_generatedFraudsterId = Lens.lens (\FraudsterSummary' {generatedFraudsterId} -> generatedFraudsterId) (\s@FraudsterSummary' {} a -> s {generatedFraudsterId = a} :: FraudsterSummary)

-- | The identifier of the watchlists the fraudster is a part of.
fraudsterSummary_watchlistIds :: Lens.Lens' FraudsterSummary (Prelude.Maybe [Prelude.Text])
fraudsterSummary_watchlistIds = Lens.lens (\FraudsterSummary' {watchlistIds} -> watchlistIds) (\s@FraudsterSummary' {} a -> s {watchlistIds = a} :: FraudsterSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FraudsterSummary where
  parseJSON =
    Data.withObject
      "FraudsterSummary"
      ( \x ->
          FraudsterSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "GeneratedFraudsterId")
            Prelude.<*> (x Data..:? "WatchlistIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FraudsterSummary where
  hashWithSalt _salt FraudsterSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` generatedFraudsterId
      `Prelude.hashWithSalt` watchlistIds

instance Prelude.NFData FraudsterSummary where
  rnf FraudsterSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf generatedFraudsterId
      `Prelude.seq` Prelude.rnf watchlistIds
