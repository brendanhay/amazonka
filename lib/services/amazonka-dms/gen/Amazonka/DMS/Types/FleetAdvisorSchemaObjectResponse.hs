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
-- Module      : Amazonka.DMS.Types.FleetAdvisorSchemaObjectResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.FleetAdvisorSchemaObjectResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a schema object in a Fleet Advisor collector inventory.
--
-- /See:/ 'newFleetAdvisorSchemaObjectResponse' smart constructor.
data FleetAdvisorSchemaObjectResponse = FleetAdvisorSchemaObjectResponse'
  { -- | The number of lines of code in a schema object in a Fleet Advisor
    -- collector inventory.
    codeLineCount :: Prelude.Maybe Prelude.Integer,
    -- | The size level of the code in a schema object in a Fleet Advisor
    -- collector inventory.
    codeSize :: Prelude.Maybe Prelude.Integer,
    -- | The number of objects in a schema object in a Fleet Advisor collector
    -- inventory.
    numberOfObjects :: Prelude.Maybe Prelude.Integer,
    -- | The type of the schema object, as reported by the database engine.
    -- Examples include the following:
    --
    -- -   @function@
    --
    -- -   @trigger@
    --
    -- -   @SYSTEM_TABLE@
    --
    -- -   @QUEUE@
    objectType :: Prelude.Maybe Prelude.Text,
    -- | The ID of a schema object in a Fleet Advisor collector inventory.
    schemaId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetAdvisorSchemaObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeLineCount', 'fleetAdvisorSchemaObjectResponse_codeLineCount' - The number of lines of code in a schema object in a Fleet Advisor
-- collector inventory.
--
-- 'codeSize', 'fleetAdvisorSchemaObjectResponse_codeSize' - The size level of the code in a schema object in a Fleet Advisor
-- collector inventory.
--
-- 'numberOfObjects', 'fleetAdvisorSchemaObjectResponse_numberOfObjects' - The number of objects in a schema object in a Fleet Advisor collector
-- inventory.
--
-- 'objectType', 'fleetAdvisorSchemaObjectResponse_objectType' - The type of the schema object, as reported by the database engine.
-- Examples include the following:
--
-- -   @function@
--
-- -   @trigger@
--
-- -   @SYSTEM_TABLE@
--
-- -   @QUEUE@
--
-- 'schemaId', 'fleetAdvisorSchemaObjectResponse_schemaId' - The ID of a schema object in a Fleet Advisor collector inventory.
newFleetAdvisorSchemaObjectResponse ::
  FleetAdvisorSchemaObjectResponse
newFleetAdvisorSchemaObjectResponse =
  FleetAdvisorSchemaObjectResponse'
    { codeLineCount =
        Prelude.Nothing,
      codeSize = Prelude.Nothing,
      numberOfObjects = Prelude.Nothing,
      objectType = Prelude.Nothing,
      schemaId = Prelude.Nothing
    }

-- | The number of lines of code in a schema object in a Fleet Advisor
-- collector inventory.
fleetAdvisorSchemaObjectResponse_codeLineCount :: Lens.Lens' FleetAdvisorSchemaObjectResponse (Prelude.Maybe Prelude.Integer)
fleetAdvisorSchemaObjectResponse_codeLineCount = Lens.lens (\FleetAdvisorSchemaObjectResponse' {codeLineCount} -> codeLineCount) (\s@FleetAdvisorSchemaObjectResponse' {} a -> s {codeLineCount = a} :: FleetAdvisorSchemaObjectResponse)

-- | The size level of the code in a schema object in a Fleet Advisor
-- collector inventory.
fleetAdvisorSchemaObjectResponse_codeSize :: Lens.Lens' FleetAdvisorSchemaObjectResponse (Prelude.Maybe Prelude.Integer)
fleetAdvisorSchemaObjectResponse_codeSize = Lens.lens (\FleetAdvisorSchemaObjectResponse' {codeSize} -> codeSize) (\s@FleetAdvisorSchemaObjectResponse' {} a -> s {codeSize = a} :: FleetAdvisorSchemaObjectResponse)

-- | The number of objects in a schema object in a Fleet Advisor collector
-- inventory.
fleetAdvisorSchemaObjectResponse_numberOfObjects :: Lens.Lens' FleetAdvisorSchemaObjectResponse (Prelude.Maybe Prelude.Integer)
fleetAdvisorSchemaObjectResponse_numberOfObjects = Lens.lens (\FleetAdvisorSchemaObjectResponse' {numberOfObjects} -> numberOfObjects) (\s@FleetAdvisorSchemaObjectResponse' {} a -> s {numberOfObjects = a} :: FleetAdvisorSchemaObjectResponse)

-- | The type of the schema object, as reported by the database engine.
-- Examples include the following:
--
-- -   @function@
--
-- -   @trigger@
--
-- -   @SYSTEM_TABLE@
--
-- -   @QUEUE@
fleetAdvisorSchemaObjectResponse_objectType :: Lens.Lens' FleetAdvisorSchemaObjectResponse (Prelude.Maybe Prelude.Text)
fleetAdvisorSchemaObjectResponse_objectType = Lens.lens (\FleetAdvisorSchemaObjectResponse' {objectType} -> objectType) (\s@FleetAdvisorSchemaObjectResponse' {} a -> s {objectType = a} :: FleetAdvisorSchemaObjectResponse)

-- | The ID of a schema object in a Fleet Advisor collector inventory.
fleetAdvisorSchemaObjectResponse_schemaId :: Lens.Lens' FleetAdvisorSchemaObjectResponse (Prelude.Maybe Prelude.Text)
fleetAdvisorSchemaObjectResponse_schemaId = Lens.lens (\FleetAdvisorSchemaObjectResponse' {schemaId} -> schemaId) (\s@FleetAdvisorSchemaObjectResponse' {} a -> s {schemaId = a} :: FleetAdvisorSchemaObjectResponse)

instance
  Data.FromJSON
    FleetAdvisorSchemaObjectResponse
  where
  parseJSON =
    Data.withObject
      "FleetAdvisorSchemaObjectResponse"
      ( \x ->
          FleetAdvisorSchemaObjectResponse'
            Prelude.<$> (x Data..:? "CodeLineCount")
            Prelude.<*> (x Data..:? "CodeSize")
            Prelude.<*> (x Data..:? "NumberOfObjects")
            Prelude.<*> (x Data..:? "ObjectType")
            Prelude.<*> (x Data..:? "SchemaId")
      )

instance
  Prelude.Hashable
    FleetAdvisorSchemaObjectResponse
  where
  hashWithSalt
    _salt
    FleetAdvisorSchemaObjectResponse' {..} =
      _salt `Prelude.hashWithSalt` codeLineCount
        `Prelude.hashWithSalt` codeSize
        `Prelude.hashWithSalt` numberOfObjects
        `Prelude.hashWithSalt` objectType
        `Prelude.hashWithSalt` schemaId

instance
  Prelude.NFData
    FleetAdvisorSchemaObjectResponse
  where
  rnf FleetAdvisorSchemaObjectResponse' {..} =
    Prelude.rnf codeLineCount
      `Prelude.seq` Prelude.rnf codeSize
      `Prelude.seq` Prelude.rnf numberOfObjects
      `Prelude.seq` Prelude.rnf objectType
      `Prelude.seq` Prelude.rnf schemaId
