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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceMetadata where

import Amazonka.AppFlow.Types.SalesforceDataTransferApi
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Salesforce.
--
-- /See:/ 'newSalesforceMetadata' smart constructor.
data SalesforceMetadata = SalesforceMetadata'
  { -- | The Salesforce APIs that you can have Amazon AppFlow use when your flows
    -- transfers data to or from Salesforce.
    dataTransferApis :: Prelude.Maybe [SalesforceDataTransferApi],
    -- | The desired authorization scope for the Salesforce account.
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
-- 'dataTransferApis', 'salesforceMetadata_dataTransferApis' - The Salesforce APIs that you can have Amazon AppFlow use when your flows
-- transfers data to or from Salesforce.
--
-- 'oAuthScopes', 'salesforceMetadata_oAuthScopes' - The desired authorization scope for the Salesforce account.
newSalesforceMetadata ::
  SalesforceMetadata
newSalesforceMetadata =
  SalesforceMetadata'
    { dataTransferApis =
        Prelude.Nothing,
      oAuthScopes = Prelude.Nothing
    }

-- | The Salesforce APIs that you can have Amazon AppFlow use when your flows
-- transfers data to or from Salesforce.
salesforceMetadata_dataTransferApis :: Lens.Lens' SalesforceMetadata (Prelude.Maybe [SalesforceDataTransferApi])
salesforceMetadata_dataTransferApis = Lens.lens (\SalesforceMetadata' {dataTransferApis} -> dataTransferApis) (\s@SalesforceMetadata' {} a -> s {dataTransferApis = a} :: SalesforceMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The desired authorization scope for the Salesforce account.
salesforceMetadata_oAuthScopes :: Lens.Lens' SalesforceMetadata (Prelude.Maybe [Prelude.Text])
salesforceMetadata_oAuthScopes = Lens.lens (\SalesforceMetadata' {oAuthScopes} -> oAuthScopes) (\s@SalesforceMetadata' {} a -> s {oAuthScopes = a} :: SalesforceMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SalesforceMetadata where
  parseJSON =
    Data.withObject
      "SalesforceMetadata"
      ( \x ->
          SalesforceMetadata'
            Prelude.<$> ( x
                            Data..:? "dataTransferApis"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "oAuthScopes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SalesforceMetadata where
  hashWithSalt _salt SalesforceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferApis
      `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData SalesforceMetadata where
  rnf SalesforceMetadata' {..} =
    Prelude.rnf dataTransferApis `Prelude.seq`
      Prelude.rnf oAuthScopes
