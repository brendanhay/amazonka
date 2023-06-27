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
-- Module      : Amazonka.ServiceCatalog.Types.ServiceActionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ServiceActionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ServiceActionDefinitionKey
import Amazonka.ServiceCatalog.Types.ServiceActionSummary

-- | An object containing detailed information about the self-service action.
--
-- /See:/ 'newServiceActionDetail' smart constructor.
data ServiceActionDetail = ServiceActionDetail'
  { -- | A map that defines the self-service action.
    definition :: Prelude.Maybe (Prelude.HashMap ServiceActionDefinitionKey Prelude.Text),
    -- | Summary information about the self-service action.
    serviceActionSummary :: Prelude.Maybe ServiceActionSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceActionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'serviceActionDetail_definition' - A map that defines the self-service action.
--
-- 'serviceActionSummary', 'serviceActionDetail_serviceActionSummary' - Summary information about the self-service action.
newServiceActionDetail ::
  ServiceActionDetail
newServiceActionDetail =
  ServiceActionDetail'
    { definition = Prelude.Nothing,
      serviceActionSummary = Prelude.Nothing
    }

-- | A map that defines the self-service action.
serviceActionDetail_definition :: Lens.Lens' ServiceActionDetail (Prelude.Maybe (Prelude.HashMap ServiceActionDefinitionKey Prelude.Text))
serviceActionDetail_definition = Lens.lens (\ServiceActionDetail' {definition} -> definition) (\s@ServiceActionDetail' {} a -> s {definition = a} :: ServiceActionDetail) Prelude.. Lens.mapping Lens.coerced

-- | Summary information about the self-service action.
serviceActionDetail_serviceActionSummary :: Lens.Lens' ServiceActionDetail (Prelude.Maybe ServiceActionSummary)
serviceActionDetail_serviceActionSummary = Lens.lens (\ServiceActionDetail' {serviceActionSummary} -> serviceActionSummary) (\s@ServiceActionDetail' {} a -> s {serviceActionSummary = a} :: ServiceActionDetail)

instance Data.FromJSON ServiceActionDetail where
  parseJSON =
    Data.withObject
      "ServiceActionDetail"
      ( \x ->
          ServiceActionDetail'
            Prelude.<$> (x Data..:? "Definition" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ServiceActionSummary")
      )

instance Prelude.Hashable ServiceActionDetail where
  hashWithSalt _salt ServiceActionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` serviceActionSummary

instance Prelude.NFData ServiceActionDetail where
  rnf ServiceActionDetail' {..} =
    Prelude.rnf definition
      `Prelude.seq` Prelude.rnf serviceActionSummary
