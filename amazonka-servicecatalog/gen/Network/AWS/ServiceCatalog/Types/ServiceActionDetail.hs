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
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary

-- | An object containing detailed information about the self-service action.
--
-- /See:/ 'newServiceActionDetail' smart constructor.
data ServiceActionDetail = ServiceActionDetail'
  { -- | Summary information about the self-service action.
    serviceActionSummary :: Core.Maybe ServiceActionSummary,
    -- | A map that defines the self-service action.
    definition :: Core.Maybe (Core.HashMap ServiceActionDefinitionKey Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceActionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionSummary', 'serviceActionDetail_serviceActionSummary' - Summary information about the self-service action.
--
-- 'definition', 'serviceActionDetail_definition' - A map that defines the self-service action.
newServiceActionDetail ::
  ServiceActionDetail
newServiceActionDetail =
  ServiceActionDetail'
    { serviceActionSummary =
        Core.Nothing,
      definition = Core.Nothing
    }

-- | Summary information about the self-service action.
serviceActionDetail_serviceActionSummary :: Lens.Lens' ServiceActionDetail (Core.Maybe ServiceActionSummary)
serviceActionDetail_serviceActionSummary = Lens.lens (\ServiceActionDetail' {serviceActionSummary} -> serviceActionSummary) (\s@ServiceActionDetail' {} a -> s {serviceActionSummary = a} :: ServiceActionDetail)

-- | A map that defines the self-service action.
serviceActionDetail_definition :: Lens.Lens' ServiceActionDetail (Core.Maybe (Core.HashMap ServiceActionDefinitionKey Core.Text))
serviceActionDetail_definition = Lens.lens (\ServiceActionDetail' {definition} -> definition) (\s@ServiceActionDetail' {} a -> s {definition = a} :: ServiceActionDetail) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ServiceActionDetail where
  parseJSON =
    Core.withObject
      "ServiceActionDetail"
      ( \x ->
          ServiceActionDetail'
            Core.<$> (x Core..:? "ServiceActionSummary")
            Core.<*> (x Core..:? "Definition" Core..!= Core.mempty)
      )

instance Core.Hashable ServiceActionDetail

instance Core.NFData ServiceActionDetail
