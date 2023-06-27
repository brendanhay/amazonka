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
-- Module      : Amazonka.Glue.Types.Blueprint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Blueprint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.BlueprintStatus
import Amazonka.Glue.Types.LastActiveDefinition
import qualified Amazonka.Prelude as Prelude

-- | The details of a blueprint.
--
-- /See:/ 'newBlueprint' smart constructor.
data Blueprint = Blueprint'
  { -- | Specifies the path in Amazon S3 where the blueprint is published.
    blueprintLocation :: Prelude.Maybe Prelude.Text,
    -- | Specifies a path in Amazon S3 where the blueprint is copied when you
    -- call @CreateBlueprint\/UpdateBlueprint@ to register the blueprint in
    -- Glue.
    blueprintServiceLocation :: Prelude.Maybe Prelude.Text,
    -- | The date and time the blueprint was registered.
    createdOn :: Prelude.Maybe Data.POSIX,
    -- | The description of the blueprint.
    description :: Prelude.Maybe Prelude.Text,
    -- | An error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | When there are multiple versions of a blueprint and the latest version
    -- has some errors, this attribute indicates the last successful blueprint
    -- definition that is available with the service.
    lastActiveDefinition :: Prelude.Maybe LastActiveDefinition,
    -- | The date and time the blueprint was last modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The name of the blueprint.
    name :: Prelude.Maybe Prelude.Text,
    -- | A JSON string that indicates the list of parameter specifications for
    -- the blueprint.
    parameterSpec :: Prelude.Maybe Prelude.Text,
    -- | The status of the blueprint registration.
    --
    -- -   Creating — The blueprint registration is in progress.
    --
    -- -   Active — The blueprint has been successfully registered.
    --
    -- -   Updating — An update to the blueprint registration is in progress.
    --
    -- -   Failed — The blueprint registration failed.
    status :: Prelude.Maybe BlueprintStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Blueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintLocation', 'blueprint_blueprintLocation' - Specifies the path in Amazon S3 where the blueprint is published.
--
-- 'blueprintServiceLocation', 'blueprint_blueprintServiceLocation' - Specifies a path in Amazon S3 where the blueprint is copied when you
-- call @CreateBlueprint\/UpdateBlueprint@ to register the blueprint in
-- Glue.
--
-- 'createdOn', 'blueprint_createdOn' - The date and time the blueprint was registered.
--
-- 'description', 'blueprint_description' - The description of the blueprint.
--
-- 'errorMessage', 'blueprint_errorMessage' - An error message.
--
-- 'lastActiveDefinition', 'blueprint_lastActiveDefinition' - When there are multiple versions of a blueprint and the latest version
-- has some errors, this attribute indicates the last successful blueprint
-- definition that is available with the service.
--
-- 'lastModifiedOn', 'blueprint_lastModifiedOn' - The date and time the blueprint was last modified.
--
-- 'name', 'blueprint_name' - The name of the blueprint.
--
-- 'parameterSpec', 'blueprint_parameterSpec' - A JSON string that indicates the list of parameter specifications for
-- the blueprint.
--
-- 'status', 'blueprint_status' - The status of the blueprint registration.
--
-- -   Creating — The blueprint registration is in progress.
--
-- -   Active — The blueprint has been successfully registered.
--
-- -   Updating — An update to the blueprint registration is in progress.
--
-- -   Failed — The blueprint registration failed.
newBlueprint ::
  Blueprint
newBlueprint =
  Blueprint'
    { blueprintLocation = Prelude.Nothing,
      blueprintServiceLocation = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      description = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      lastActiveDefinition = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      name = Prelude.Nothing,
      parameterSpec = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the path in Amazon S3 where the blueprint is published.
blueprint_blueprintLocation :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_blueprintLocation = Lens.lens (\Blueprint' {blueprintLocation} -> blueprintLocation) (\s@Blueprint' {} a -> s {blueprintLocation = a} :: Blueprint)

-- | Specifies a path in Amazon S3 where the blueprint is copied when you
-- call @CreateBlueprint\/UpdateBlueprint@ to register the blueprint in
-- Glue.
blueprint_blueprintServiceLocation :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_blueprintServiceLocation = Lens.lens (\Blueprint' {blueprintServiceLocation} -> blueprintServiceLocation) (\s@Blueprint' {} a -> s {blueprintServiceLocation = a} :: Blueprint)

-- | The date and time the blueprint was registered.
blueprint_createdOn :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.UTCTime)
blueprint_createdOn = Lens.lens (\Blueprint' {createdOn} -> createdOn) (\s@Blueprint' {} a -> s {createdOn = a} :: Blueprint) Prelude.. Lens.mapping Data._Time

-- | The description of the blueprint.
blueprint_description :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_description = Lens.lens (\Blueprint' {description} -> description) (\s@Blueprint' {} a -> s {description = a} :: Blueprint)

-- | An error message.
blueprint_errorMessage :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_errorMessage = Lens.lens (\Blueprint' {errorMessage} -> errorMessage) (\s@Blueprint' {} a -> s {errorMessage = a} :: Blueprint)

-- | When there are multiple versions of a blueprint and the latest version
-- has some errors, this attribute indicates the last successful blueprint
-- definition that is available with the service.
blueprint_lastActiveDefinition :: Lens.Lens' Blueprint (Prelude.Maybe LastActiveDefinition)
blueprint_lastActiveDefinition = Lens.lens (\Blueprint' {lastActiveDefinition} -> lastActiveDefinition) (\s@Blueprint' {} a -> s {lastActiveDefinition = a} :: Blueprint)

-- | The date and time the blueprint was last modified.
blueprint_lastModifiedOn :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.UTCTime)
blueprint_lastModifiedOn = Lens.lens (\Blueprint' {lastModifiedOn} -> lastModifiedOn) (\s@Blueprint' {} a -> s {lastModifiedOn = a} :: Blueprint) Prelude.. Lens.mapping Data._Time

-- | The name of the blueprint.
blueprint_name :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_name = Lens.lens (\Blueprint' {name} -> name) (\s@Blueprint' {} a -> s {name = a} :: Blueprint)

-- | A JSON string that indicates the list of parameter specifications for
-- the blueprint.
blueprint_parameterSpec :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_parameterSpec = Lens.lens (\Blueprint' {parameterSpec} -> parameterSpec) (\s@Blueprint' {} a -> s {parameterSpec = a} :: Blueprint)

-- | The status of the blueprint registration.
--
-- -   Creating — The blueprint registration is in progress.
--
-- -   Active — The blueprint has been successfully registered.
--
-- -   Updating — An update to the blueprint registration is in progress.
--
-- -   Failed — The blueprint registration failed.
blueprint_status :: Lens.Lens' Blueprint (Prelude.Maybe BlueprintStatus)
blueprint_status = Lens.lens (\Blueprint' {status} -> status) (\s@Blueprint' {} a -> s {status = a} :: Blueprint)

instance Data.FromJSON Blueprint where
  parseJSON =
    Data.withObject
      "Blueprint"
      ( \x ->
          Blueprint'
            Prelude.<$> (x Data..:? "BlueprintLocation")
            Prelude.<*> (x Data..:? "BlueprintServiceLocation")
            Prelude.<*> (x Data..:? "CreatedOn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastActiveDefinition")
            Prelude.<*> (x Data..:? "LastModifiedOn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ParameterSpec")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Blueprint where
  hashWithSalt _salt Blueprint' {..} =
    _salt
      `Prelude.hashWithSalt` blueprintLocation
      `Prelude.hashWithSalt` blueprintServiceLocation
      `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` lastActiveDefinition
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parameterSpec
      `Prelude.hashWithSalt` status

instance Prelude.NFData Blueprint where
  rnf Blueprint' {..} =
    Prelude.rnf blueprintLocation
      `Prelude.seq` Prelude.rnf blueprintServiceLocation
      `Prelude.seq` Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastActiveDefinition
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parameterSpec
      `Prelude.seq` Prelude.rnf status
