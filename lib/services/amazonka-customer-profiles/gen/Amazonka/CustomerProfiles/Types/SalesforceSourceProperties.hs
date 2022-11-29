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
-- Module      : Amazonka.CustomerProfiles.Types.SalesforceSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.SalesforceSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Salesforce is being used as a
-- source.
--
-- /See:/ 'newSalesforceSourceProperties' smart constructor.
data SalesforceSourceProperties = SalesforceSourceProperties'
  { -- | Indicates whether Amazon AppFlow includes deleted files in the flow run.
    includeDeletedRecords :: Prelude.Maybe Prelude.Bool,
    -- | The flag that enables dynamic fetching of new (recently added) fields in
    -- the Salesforce objects while running a flow.
    enableDynamicFieldUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The object specified in the Salesforce flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeDeletedRecords', 'salesforceSourceProperties_includeDeletedRecords' - Indicates whether Amazon AppFlow includes deleted files in the flow run.
--
-- 'enableDynamicFieldUpdate', 'salesforceSourceProperties_enableDynamicFieldUpdate' - The flag that enables dynamic fetching of new (recently added) fields in
-- the Salesforce objects while running a flow.
--
-- 'object'', 'salesforceSourceProperties_object' - The object specified in the Salesforce flow source.
newSalesforceSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  SalesforceSourceProperties
newSalesforceSourceProperties pObject_ =
  SalesforceSourceProperties'
    { includeDeletedRecords =
        Prelude.Nothing,
      enableDynamicFieldUpdate = Prelude.Nothing,
      object' = pObject_
    }

-- | Indicates whether Amazon AppFlow includes deleted files in the flow run.
salesforceSourceProperties_includeDeletedRecords :: Lens.Lens' SalesforceSourceProperties (Prelude.Maybe Prelude.Bool)
salesforceSourceProperties_includeDeletedRecords = Lens.lens (\SalesforceSourceProperties' {includeDeletedRecords} -> includeDeletedRecords) (\s@SalesforceSourceProperties' {} a -> s {includeDeletedRecords = a} :: SalesforceSourceProperties)

-- | The flag that enables dynamic fetching of new (recently added) fields in
-- the Salesforce objects while running a flow.
salesforceSourceProperties_enableDynamicFieldUpdate :: Lens.Lens' SalesforceSourceProperties (Prelude.Maybe Prelude.Bool)
salesforceSourceProperties_enableDynamicFieldUpdate = Lens.lens (\SalesforceSourceProperties' {enableDynamicFieldUpdate} -> enableDynamicFieldUpdate) (\s@SalesforceSourceProperties' {} a -> s {enableDynamicFieldUpdate = a} :: SalesforceSourceProperties)

-- | The object specified in the Salesforce flow source.
salesforceSourceProperties_object :: Lens.Lens' SalesforceSourceProperties Prelude.Text
salesforceSourceProperties_object = Lens.lens (\SalesforceSourceProperties' {object'} -> object') (\s@SalesforceSourceProperties' {} a -> s {object' = a} :: SalesforceSourceProperties)

instance Prelude.Hashable SalesforceSourceProperties where
  hashWithSalt _salt SalesforceSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` includeDeletedRecords
      `Prelude.hashWithSalt` enableDynamicFieldUpdate
      `Prelude.hashWithSalt` object'

instance Prelude.NFData SalesforceSourceProperties where
  rnf SalesforceSourceProperties' {..} =
    Prelude.rnf includeDeletedRecords
      `Prelude.seq` Prelude.rnf enableDynamicFieldUpdate
      `Prelude.seq` Prelude.rnf object'

instance Core.ToJSON SalesforceSourceProperties where
  toJSON SalesforceSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeDeletedRecords" Core..=)
              Prelude.<$> includeDeletedRecords,
            ("EnableDynamicFieldUpdate" Core..=)
              Prelude.<$> enableDynamicFieldUpdate,
            Prelude.Just ("Object" Core..= object')
          ]
      )
