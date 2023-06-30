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
-- Module      : Amazonka.CustomerProfiles.Types.ConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ConnectorOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.MarketoConnectorOperator
import Amazonka.CustomerProfiles.Types.S3ConnectorOperator
import Amazonka.CustomerProfiles.Types.SalesforceConnectorOperator
import Amazonka.CustomerProfiles.Types.ServiceNowConnectorOperator
import Amazonka.CustomerProfiles.Types.ZendeskConnectorOperator
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The operation to be performed on the provided source fields.
--
-- /See:/ 'newConnectorOperator' smart constructor.
data ConnectorOperator = ConnectorOperator'
  { -- | The operation to be performed on the provided Marketo source fields.
    marketo :: Prelude.Maybe MarketoConnectorOperator,
    -- | The operation to be performed on the provided Amazon S3 source fields.
    s3 :: Prelude.Maybe S3ConnectorOperator,
    -- | The operation to be performed on the provided Salesforce source fields.
    salesforce :: Prelude.Maybe SalesforceConnectorOperator,
    -- | The operation to be performed on the provided ServiceNow source fields.
    serviceNow :: Prelude.Maybe ServiceNowConnectorOperator,
    -- | The operation to be performed on the provided Zendesk source fields.
    zendesk :: Prelude.Maybe ZendeskConnectorOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marketo', 'connectorOperator_marketo' - The operation to be performed on the provided Marketo source fields.
--
-- 's3', 'connectorOperator_s3' - The operation to be performed on the provided Amazon S3 source fields.
--
-- 'salesforce', 'connectorOperator_salesforce' - The operation to be performed on the provided Salesforce source fields.
--
-- 'serviceNow', 'connectorOperator_serviceNow' - The operation to be performed on the provided ServiceNow source fields.
--
-- 'zendesk', 'connectorOperator_zendesk' - The operation to be performed on the provided Zendesk source fields.
newConnectorOperator ::
  ConnectorOperator
newConnectorOperator =
  ConnectorOperator'
    { marketo = Prelude.Nothing,
      s3 = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      zendesk = Prelude.Nothing
    }

-- | The operation to be performed on the provided Marketo source fields.
connectorOperator_marketo :: Lens.Lens' ConnectorOperator (Prelude.Maybe MarketoConnectorOperator)
connectorOperator_marketo = Lens.lens (\ConnectorOperator' {marketo} -> marketo) (\s@ConnectorOperator' {} a -> s {marketo = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Amazon S3 source fields.
connectorOperator_s3 :: Lens.Lens' ConnectorOperator (Prelude.Maybe S3ConnectorOperator)
connectorOperator_s3 = Lens.lens (\ConnectorOperator' {s3} -> s3) (\s@ConnectorOperator' {} a -> s {s3 = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Salesforce source fields.
connectorOperator_salesforce :: Lens.Lens' ConnectorOperator (Prelude.Maybe SalesforceConnectorOperator)
connectorOperator_salesforce = Lens.lens (\ConnectorOperator' {salesforce} -> salesforce) (\s@ConnectorOperator' {} a -> s {salesforce = a} :: ConnectorOperator)

-- | The operation to be performed on the provided ServiceNow source fields.
connectorOperator_serviceNow :: Lens.Lens' ConnectorOperator (Prelude.Maybe ServiceNowConnectorOperator)
connectorOperator_serviceNow = Lens.lens (\ConnectorOperator' {serviceNow} -> serviceNow) (\s@ConnectorOperator' {} a -> s {serviceNow = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Zendesk source fields.
connectorOperator_zendesk :: Lens.Lens' ConnectorOperator (Prelude.Maybe ZendeskConnectorOperator)
connectorOperator_zendesk = Lens.lens (\ConnectorOperator' {zendesk} -> zendesk) (\s@ConnectorOperator' {} a -> s {zendesk = a} :: ConnectorOperator)

instance Prelude.Hashable ConnectorOperator where
  hashWithSalt _salt ConnectorOperator' {..} =
    _salt
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` zendesk

instance Prelude.NFData ConnectorOperator where
  rnf ConnectorOperator' {..} =
    Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf zendesk

instance Data.ToJSON ConnectorOperator where
  toJSON ConnectorOperator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marketo" Data..=) Prelude.<$> marketo,
            ("S3" Data..=) Prelude.<$> s3,
            ("Salesforce" Data..=) Prelude.<$> salesforce,
            ("ServiceNow" Data..=) Prelude.<$> serviceNow,
            ("Zendesk" Data..=) Prelude.<$> zendesk
          ]
      )
