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
-- Module      : Amazonka.AppFlow.Types.SalesforceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Salesforce.
--
-- /See:/ 'newSalesforceMetadata' smart constructor.
data SalesforceMetadata = SalesforceMetadata'
  { -- | The desired authorization scope for the Salesforce account.
    oAuthScopes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthScopes', 'salesforceMetadata_oAuthScopes' - The desired authorization scope for the Salesforce account.
newSalesforceMetadata ::
  SalesforceMetadata
newSalesforceMetadata =
  SalesforceMetadata' {oAuthScopes = Prelude.Nothing}

-- | The desired authorization scope for the Salesforce account.
salesforceMetadata_oAuthScopes :: Lens.Lens' SalesforceMetadata (Prelude.Maybe [Prelude.Text])
salesforceMetadata_oAuthScopes = Lens.lens (\SalesforceMetadata' {oAuthScopes} -> oAuthScopes) (\s@SalesforceMetadata' {} a -> s {oAuthScopes = a} :: SalesforceMetadata) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SalesforceMetadata where
  parseJSON =
    Core.withObject
      "SalesforceMetadata"
      ( \x ->
          SalesforceMetadata'
            Prelude.<$> (x Core..:? "oAuthScopes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SalesforceMetadata where
  hashWithSalt _salt SalesforceMetadata' {..} =
    _salt `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData SalesforceMetadata where
  rnf SalesforceMetadata' {..} = Prelude.rnf oAuthScopes
