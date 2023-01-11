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
-- Module      : Amazonka.IoTFleetWise.Types.CollectionScheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CollectionScheme where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.ConditionBasedCollectionScheme
import Amazonka.IoTFleetWise.Types.TimeBasedCollectionScheme
import qualified Amazonka.Prelude as Prelude

-- | Specifies what data to collect and how often or when to collect it.
--
-- /See:/ 'newCollectionScheme' smart constructor.
data CollectionScheme = CollectionScheme'
  { -- | Information about a collection scheme that uses a simple logical
    -- expression to recognize what data to collect.
    conditionBasedCollectionScheme :: Prelude.Maybe ConditionBasedCollectionScheme,
    -- | Information about a collection scheme that uses a time period to decide
    -- how often to collect data.
    timeBasedCollectionScheme :: Prelude.Maybe TimeBasedCollectionScheme
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectionScheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionBasedCollectionScheme', 'collectionScheme_conditionBasedCollectionScheme' - Information about a collection scheme that uses a simple logical
-- expression to recognize what data to collect.
--
-- 'timeBasedCollectionScheme', 'collectionScheme_timeBasedCollectionScheme' - Information about a collection scheme that uses a time period to decide
-- how often to collect data.
newCollectionScheme ::
  CollectionScheme
newCollectionScheme =
  CollectionScheme'
    { conditionBasedCollectionScheme =
        Prelude.Nothing,
      timeBasedCollectionScheme = Prelude.Nothing
    }

-- | Information about a collection scheme that uses a simple logical
-- expression to recognize what data to collect.
collectionScheme_conditionBasedCollectionScheme :: Lens.Lens' CollectionScheme (Prelude.Maybe ConditionBasedCollectionScheme)
collectionScheme_conditionBasedCollectionScheme = Lens.lens (\CollectionScheme' {conditionBasedCollectionScheme} -> conditionBasedCollectionScheme) (\s@CollectionScheme' {} a -> s {conditionBasedCollectionScheme = a} :: CollectionScheme)

-- | Information about a collection scheme that uses a time period to decide
-- how often to collect data.
collectionScheme_timeBasedCollectionScheme :: Lens.Lens' CollectionScheme (Prelude.Maybe TimeBasedCollectionScheme)
collectionScheme_timeBasedCollectionScheme = Lens.lens (\CollectionScheme' {timeBasedCollectionScheme} -> timeBasedCollectionScheme) (\s@CollectionScheme' {} a -> s {timeBasedCollectionScheme = a} :: CollectionScheme)

instance Data.FromJSON CollectionScheme where
  parseJSON =
    Data.withObject
      "CollectionScheme"
      ( \x ->
          CollectionScheme'
            Prelude.<$> (x Data..:? "conditionBasedCollectionScheme")
            Prelude.<*> (x Data..:? "timeBasedCollectionScheme")
      )

instance Prelude.Hashable CollectionScheme where
  hashWithSalt _salt CollectionScheme' {..} =
    _salt
      `Prelude.hashWithSalt` conditionBasedCollectionScheme
      `Prelude.hashWithSalt` timeBasedCollectionScheme

instance Prelude.NFData CollectionScheme where
  rnf CollectionScheme' {..} =
    Prelude.rnf conditionBasedCollectionScheme
      `Prelude.seq` Prelude.rnf timeBasedCollectionScheme

instance Data.ToJSON CollectionScheme where
  toJSON CollectionScheme' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conditionBasedCollectionScheme" Data..=)
              Prelude.<$> conditionBasedCollectionScheme,
            ("timeBasedCollectionScheme" Data..=)
              Prelude.<$> timeBasedCollectionScheme
          ]
      )
