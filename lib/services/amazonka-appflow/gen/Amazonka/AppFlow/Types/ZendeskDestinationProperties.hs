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
-- Module      : Amazonka.AppFlow.Types.ZendeskDestinationProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ZendeskDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import Amazonka.AppFlow.Types.WriteOperationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Zendesk is used as a destination.
--
-- /See:/ 'newZendeskDestinationProperties' smart constructor.
data ZendeskDestinationProperties = ZendeskDestinationProperties'
  { errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    idFieldNames :: Prelude.Maybe [Prelude.Text],
    writeOperationType :: Prelude.Maybe WriteOperationType,
    -- | The object specified in the Zendesk flow destination.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZendeskDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorHandlingConfig', 'zendeskDestinationProperties_errorHandlingConfig' - Undocumented member.
--
-- 'idFieldNames', 'zendeskDestinationProperties_idFieldNames' - Undocumented member.
--
-- 'writeOperationType', 'zendeskDestinationProperties_writeOperationType' - Undocumented member.
--
-- 'object'', 'zendeskDestinationProperties_object' - The object specified in the Zendesk flow destination.
newZendeskDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  ZendeskDestinationProperties
newZendeskDestinationProperties pObject_ =
  ZendeskDestinationProperties'
    { errorHandlingConfig =
        Prelude.Nothing,
      idFieldNames = Prelude.Nothing,
      writeOperationType = Prelude.Nothing,
      object' = pObject_
    }

-- | Undocumented member.
zendeskDestinationProperties_errorHandlingConfig :: Lens.Lens' ZendeskDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
zendeskDestinationProperties_errorHandlingConfig = Lens.lens (\ZendeskDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@ZendeskDestinationProperties' {} a -> s {errorHandlingConfig = a} :: ZendeskDestinationProperties)

-- | Undocumented member.
zendeskDestinationProperties_idFieldNames :: Lens.Lens' ZendeskDestinationProperties (Prelude.Maybe [Prelude.Text])
zendeskDestinationProperties_idFieldNames = Lens.lens (\ZendeskDestinationProperties' {idFieldNames} -> idFieldNames) (\s@ZendeskDestinationProperties' {} a -> s {idFieldNames = a} :: ZendeskDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
zendeskDestinationProperties_writeOperationType :: Lens.Lens' ZendeskDestinationProperties (Prelude.Maybe WriteOperationType)
zendeskDestinationProperties_writeOperationType = Lens.lens (\ZendeskDestinationProperties' {writeOperationType} -> writeOperationType) (\s@ZendeskDestinationProperties' {} a -> s {writeOperationType = a} :: ZendeskDestinationProperties)

-- | The object specified in the Zendesk flow destination.
zendeskDestinationProperties_object :: Lens.Lens' ZendeskDestinationProperties Prelude.Text
zendeskDestinationProperties_object = Lens.lens (\ZendeskDestinationProperties' {object'} -> object') (\s@ZendeskDestinationProperties' {} a -> s {object' = a} :: ZendeskDestinationProperties)

instance Data.FromJSON ZendeskDestinationProperties where
  parseJSON =
    Data.withObject
      "ZendeskDestinationProperties"
      ( \x ->
          ZendeskDestinationProperties'
            Prelude.<$> (x Data..:? "errorHandlingConfig")
            Prelude.<*> (x Data..:? "idFieldNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "writeOperationType")
            Prelude.<*> (x Data..: "object")
      )

instance
  Prelude.Hashable
    ZendeskDestinationProperties
  where
  hashWithSalt _salt ZendeskDestinationProperties' {..} =
    _salt
      `Prelude.hashWithSalt` errorHandlingConfig
      `Prelude.hashWithSalt` idFieldNames
      `Prelude.hashWithSalt` writeOperationType
      `Prelude.hashWithSalt` object'

instance Prelude.NFData ZendeskDestinationProperties where
  rnf ZendeskDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig
      `Prelude.seq` Prelude.rnf idFieldNames
      `Prelude.seq` Prelude.rnf writeOperationType
      `Prelude.seq` Prelude.rnf object'

instance Data.ToJSON ZendeskDestinationProperties where
  toJSON ZendeskDestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Data..=)
              Prelude.<$> errorHandlingConfig,
            ("idFieldNames" Data..=) Prelude.<$> idFieldNames,
            ("writeOperationType" Data..=)
              Prelude.<$> writeOperationType,
            Prelude.Just ("object" Data..= object')
          ]
      )
