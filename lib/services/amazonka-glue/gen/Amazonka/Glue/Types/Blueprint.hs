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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Blueprint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.BlueprintStatus
import Amazonka.Glue.Types.LastActiveDefinition
import qualified Amazonka.Prelude as Prelude

-- | The details of a blueprint.
--
-- /See:/ 'newBlueprint' smart constructor.
data Blueprint = Blueprint'
  { -- | The date and time the blueprint was registered.
    createdOn :: Prelude.Maybe Core.POSIX,
    -- | The name of the blueprint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time the blueprint was last modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | An error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | When there are multiple versions of a blueprint and the latest version
    -- has some errors, this attribute indicates the last successful blueprint
    -- definition that is available with the service.
    lastActiveDefinition :: Prelude.Maybe LastActiveDefinition,
    -- | The status of the blueprint registration.
    --
    -- -   Creating — The blueprint registration is in progress.
    --
    -- -   Active — The blueprint has been successfully registered.
    --
    -- -   Updating — An update to the blueprint registration is in progress.
    --
    -- -   Failed — The blueprint registration failed.
    status :: Prelude.Maybe BlueprintStatus,
    -- | A JSON string that indicates the list of parameter specifications for
    -- the blueprint.
    parameterSpec :: Prelude.Maybe Prelude.Text,
    -- | The description of the blueprint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the path in Amazon S3 where the blueprint is published.
    blueprintLocation :: Prelude.Maybe Prelude.Text,
    -- | Specifies a path in Amazon S3 where the blueprint is copied when you
    -- call @CreateBlueprint\/UpdateBlueprint@ to register the blueprint in
    -- Glue.
    blueprintServiceLocation :: Prelude.Maybe Prelude.Text
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
-- 'createdOn', 'blueprint_createdOn' - The date and time the blueprint was registered.
--
-- 'name', 'blueprint_name' - The name of the blueprint.
--
-- 'lastModifiedOn', 'blueprint_lastModifiedOn' - The date and time the blueprint was last modified.
--
-- 'errorMessage', 'blueprint_errorMessage' - An error message.
--
-- 'lastActiveDefinition', 'blueprint_lastActiveDefinition' - When there are multiple versions of a blueprint and the latest version
-- has some errors, this attribute indicates the last successful blueprint
-- definition that is available with the service.
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
--
-- 'parameterSpec', 'blueprint_parameterSpec' - A JSON string that indicates the list of parameter specifications for
-- the blueprint.
--
-- 'description', 'blueprint_description' - The description of the blueprint.
--
-- 'blueprintLocation', 'blueprint_blueprintLocation' - Specifies the path in Amazon S3 where the blueprint is published.
--
-- 'blueprintServiceLocation', 'blueprint_blueprintServiceLocation' - Specifies a path in Amazon S3 where the blueprint is copied when you
-- call @CreateBlueprint\/UpdateBlueprint@ to register the blueprint in
-- Glue.
newBlueprint ::
  Blueprint
newBlueprint =
  Blueprint'
    { createdOn = Prelude.Nothing,
      name = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      lastActiveDefinition = Prelude.Nothing,
      status = Prelude.Nothing,
      parameterSpec = Prelude.Nothing,
      description = Prelude.Nothing,
      blueprintLocation = Prelude.Nothing,
      blueprintServiceLocation = Prelude.Nothing
    }

-- | The date and time the blueprint was registered.
blueprint_createdOn :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.UTCTime)
blueprint_createdOn = Lens.lens (\Blueprint' {createdOn} -> createdOn) (\s@Blueprint' {} a -> s {createdOn = a} :: Blueprint) Prelude.. Lens.mapping Core._Time

-- | The name of the blueprint.
blueprint_name :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_name = Lens.lens (\Blueprint' {name} -> name) (\s@Blueprint' {} a -> s {name = a} :: Blueprint)

-- | The date and time the blueprint was last modified.
blueprint_lastModifiedOn :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.UTCTime)
blueprint_lastModifiedOn = Lens.lens (\Blueprint' {lastModifiedOn} -> lastModifiedOn) (\s@Blueprint' {} a -> s {lastModifiedOn = a} :: Blueprint) Prelude.. Lens.mapping Core._Time

-- | An error message.
blueprint_errorMessage :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_errorMessage = Lens.lens (\Blueprint' {errorMessage} -> errorMessage) (\s@Blueprint' {} a -> s {errorMessage = a} :: Blueprint)

-- | When there are multiple versions of a blueprint and the latest version
-- has some errors, this attribute indicates the last successful blueprint
-- definition that is available with the service.
blueprint_lastActiveDefinition :: Lens.Lens' Blueprint (Prelude.Maybe LastActiveDefinition)
blueprint_lastActiveDefinition = Lens.lens (\Blueprint' {lastActiveDefinition} -> lastActiveDefinition) (\s@Blueprint' {} a -> s {lastActiveDefinition = a} :: Blueprint)

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

-- | A JSON string that indicates the list of parameter specifications for
-- the blueprint.
blueprint_parameterSpec :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_parameterSpec = Lens.lens (\Blueprint' {parameterSpec} -> parameterSpec) (\s@Blueprint' {} a -> s {parameterSpec = a} :: Blueprint)

-- | The description of the blueprint.
blueprint_description :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_description = Lens.lens (\Blueprint' {description} -> description) (\s@Blueprint' {} a -> s {description = a} :: Blueprint)

-- | Specifies the path in Amazon S3 where the blueprint is published.
blueprint_blueprintLocation :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_blueprintLocation = Lens.lens (\Blueprint' {blueprintLocation} -> blueprintLocation) (\s@Blueprint' {} a -> s {blueprintLocation = a} :: Blueprint)

-- | Specifies a path in Amazon S3 where the blueprint is copied when you
-- call @CreateBlueprint\/UpdateBlueprint@ to register the blueprint in
-- Glue.
blueprint_blueprintServiceLocation :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_blueprintServiceLocation = Lens.lens (\Blueprint' {blueprintServiceLocation} -> blueprintServiceLocation) (\s@Blueprint' {} a -> s {blueprintServiceLocation = a} :: Blueprint)

instance Core.FromJSON Blueprint where
  parseJSON =
    Core.withObject
      "Blueprint"
      ( \x ->
          Blueprint'
            Prelude.<$> (x Core..:? "CreatedOn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastModifiedOn")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "LastActiveDefinition")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ParameterSpec")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "BlueprintLocation")
            Prelude.<*> (x Core..:? "BlueprintServiceLocation")
      )

instance Prelude.Hashable Blueprint where
  hashWithSalt _salt Blueprint' {..} =
    _salt `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` lastActiveDefinition
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` parameterSpec
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` blueprintLocation
      `Prelude.hashWithSalt` blueprintServiceLocation

instance Prelude.NFData Blueprint where
  rnf Blueprint' {..} =
    Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastActiveDefinition
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf parameterSpec
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf blueprintLocation
      `Prelude.seq` Prelude.rnf blueprintServiceLocation
