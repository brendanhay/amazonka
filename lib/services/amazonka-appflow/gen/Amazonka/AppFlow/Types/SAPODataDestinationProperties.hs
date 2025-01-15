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
-- Module      : Amazonka.AppFlow.Types.SAPODataDestinationProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SAPODataDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import Amazonka.AppFlow.Types.SuccessResponseHandlingConfig
import Amazonka.AppFlow.Types.WriteOperationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when using SAPOData as a flow
-- destination
--
-- /See:/ 'newSAPODataDestinationProperties' smart constructor.
data SAPODataDestinationProperties = SAPODataDestinationProperties'
  { errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    idFieldNames :: Prelude.Maybe [Prelude.Text],
    -- | Determines how Amazon AppFlow handles the success response that it gets
    -- from the connector after placing data.
    --
    -- For example, this setting would determine where to write the response
    -- from a destination connector upon a successful insert operation.
    successResponseHandlingConfig :: Prelude.Maybe SuccessResponseHandlingConfig,
    writeOperationType :: Prelude.Maybe WriteOperationType,
    -- | The object path specified in the SAPOData flow destination.
    objectPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAPODataDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorHandlingConfig', 'sAPODataDestinationProperties_errorHandlingConfig' - Undocumented member.
--
-- 'idFieldNames', 'sAPODataDestinationProperties_idFieldNames' - Undocumented member.
--
-- 'successResponseHandlingConfig', 'sAPODataDestinationProperties_successResponseHandlingConfig' - Determines how Amazon AppFlow handles the success response that it gets
-- from the connector after placing data.
--
-- For example, this setting would determine where to write the response
-- from a destination connector upon a successful insert operation.
--
-- 'writeOperationType', 'sAPODataDestinationProperties_writeOperationType' - Undocumented member.
--
-- 'objectPath', 'sAPODataDestinationProperties_objectPath' - The object path specified in the SAPOData flow destination.
newSAPODataDestinationProperties ::
  -- | 'objectPath'
  Prelude.Text ->
  SAPODataDestinationProperties
newSAPODataDestinationProperties pObjectPath_ =
  SAPODataDestinationProperties'
    { errorHandlingConfig =
        Prelude.Nothing,
      idFieldNames = Prelude.Nothing,
      successResponseHandlingConfig =
        Prelude.Nothing,
      writeOperationType = Prelude.Nothing,
      objectPath = pObjectPath_
    }

-- | Undocumented member.
sAPODataDestinationProperties_errorHandlingConfig :: Lens.Lens' SAPODataDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
sAPODataDestinationProperties_errorHandlingConfig = Lens.lens (\SAPODataDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@SAPODataDestinationProperties' {} a -> s {errorHandlingConfig = a} :: SAPODataDestinationProperties)

-- | Undocumented member.
sAPODataDestinationProperties_idFieldNames :: Lens.Lens' SAPODataDestinationProperties (Prelude.Maybe [Prelude.Text])
sAPODataDestinationProperties_idFieldNames = Lens.lens (\SAPODataDestinationProperties' {idFieldNames} -> idFieldNames) (\s@SAPODataDestinationProperties' {} a -> s {idFieldNames = a} :: SAPODataDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | Determines how Amazon AppFlow handles the success response that it gets
-- from the connector after placing data.
--
-- For example, this setting would determine where to write the response
-- from a destination connector upon a successful insert operation.
sAPODataDestinationProperties_successResponseHandlingConfig :: Lens.Lens' SAPODataDestinationProperties (Prelude.Maybe SuccessResponseHandlingConfig)
sAPODataDestinationProperties_successResponseHandlingConfig = Lens.lens (\SAPODataDestinationProperties' {successResponseHandlingConfig} -> successResponseHandlingConfig) (\s@SAPODataDestinationProperties' {} a -> s {successResponseHandlingConfig = a} :: SAPODataDestinationProperties)

-- | Undocumented member.
sAPODataDestinationProperties_writeOperationType :: Lens.Lens' SAPODataDestinationProperties (Prelude.Maybe WriteOperationType)
sAPODataDestinationProperties_writeOperationType = Lens.lens (\SAPODataDestinationProperties' {writeOperationType} -> writeOperationType) (\s@SAPODataDestinationProperties' {} a -> s {writeOperationType = a} :: SAPODataDestinationProperties)

-- | The object path specified in the SAPOData flow destination.
sAPODataDestinationProperties_objectPath :: Lens.Lens' SAPODataDestinationProperties Prelude.Text
sAPODataDestinationProperties_objectPath = Lens.lens (\SAPODataDestinationProperties' {objectPath} -> objectPath) (\s@SAPODataDestinationProperties' {} a -> s {objectPath = a} :: SAPODataDestinationProperties)

instance Data.FromJSON SAPODataDestinationProperties where
  parseJSON =
    Data.withObject
      "SAPODataDestinationProperties"
      ( \x ->
          SAPODataDestinationProperties'
            Prelude.<$> (x Data..:? "errorHandlingConfig")
            Prelude.<*> (x Data..:? "idFieldNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "successResponseHandlingConfig")
            Prelude.<*> (x Data..:? "writeOperationType")
            Prelude.<*> (x Data..: "objectPath")
      )

instance
  Prelude.Hashable
    SAPODataDestinationProperties
  where
  hashWithSalt _salt SAPODataDestinationProperties' {..} =
    _salt
      `Prelude.hashWithSalt` errorHandlingConfig
      `Prelude.hashWithSalt` idFieldNames
      `Prelude.hashWithSalt` successResponseHandlingConfig
      `Prelude.hashWithSalt` writeOperationType
      `Prelude.hashWithSalt` objectPath

instance Prelude.NFData SAPODataDestinationProperties where
  rnf SAPODataDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig `Prelude.seq`
      Prelude.rnf idFieldNames `Prelude.seq`
        Prelude.rnf successResponseHandlingConfig `Prelude.seq`
          Prelude.rnf writeOperationType `Prelude.seq`
            Prelude.rnf objectPath

instance Data.ToJSON SAPODataDestinationProperties where
  toJSON SAPODataDestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Data..=)
              Prelude.<$> errorHandlingConfig,
            ("idFieldNames" Data..=) Prelude.<$> idFieldNames,
            ("successResponseHandlingConfig" Data..=)
              Prelude.<$> successResponseHandlingConfig,
            ("writeOperationType" Data..=)
              Prelude.<$> writeOperationType,
            Prelude.Just ("objectPath" Data..= objectPath)
          ]
      )
