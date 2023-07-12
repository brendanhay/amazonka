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
-- Module      : Amazonka.Glacier.Types.DataRetrievalPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.DataRetrievalPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.DataRetrievalRule
import qualified Amazonka.Prelude as Prelude

-- | Data retrieval policy.
--
-- /See:/ 'newDataRetrievalPolicy' smart constructor.
data DataRetrievalPolicy = DataRetrievalPolicy'
  { -- | The policy rule. Although this is a list type, currently there must be
    -- only one rule, which contains a Strategy field and optionally a
    -- BytesPerHour field.
    rules :: Prelude.Maybe [DataRetrievalRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRetrievalPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'dataRetrievalPolicy_rules' - The policy rule. Although this is a list type, currently there must be
-- only one rule, which contains a Strategy field and optionally a
-- BytesPerHour field.
newDataRetrievalPolicy ::
  DataRetrievalPolicy
newDataRetrievalPolicy =
  DataRetrievalPolicy' {rules = Prelude.Nothing}

-- | The policy rule. Although this is a list type, currently there must be
-- only one rule, which contains a Strategy field and optionally a
-- BytesPerHour field.
dataRetrievalPolicy_rules :: Lens.Lens' DataRetrievalPolicy (Prelude.Maybe [DataRetrievalRule])
dataRetrievalPolicy_rules = Lens.lens (\DataRetrievalPolicy' {rules} -> rules) (\s@DataRetrievalPolicy' {} a -> s {rules = a} :: DataRetrievalPolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataRetrievalPolicy where
  parseJSON =
    Data.withObject
      "DataRetrievalPolicy"
      ( \x ->
          DataRetrievalPolicy'
            Prelude.<$> (x Data..:? "Rules" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DataRetrievalPolicy where
  hashWithSalt _salt DataRetrievalPolicy' {..} =
    _salt `Prelude.hashWithSalt` rules

instance Prelude.NFData DataRetrievalPolicy where
  rnf DataRetrievalPolicy' {..} = Prelude.rnf rules

instance Data.ToJSON DataRetrievalPolicy where
  toJSON DataRetrievalPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Rules" Data..=) Prelude.<$> rules]
      )
