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
-- Module      : Amazonka.Nimble.Types.StudioComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.ScriptParameterKeyValue
import Amazonka.Nimble.Types.StudioComponentConfiguration
import Amazonka.Nimble.Types.StudioComponentInitializationScript
import Amazonka.Nimble.Types.StudioComponentState
import Amazonka.Nimble.Types.StudioComponentStatusCode
import Amazonka.Nimble.Types.StudioComponentSubtype
import Amazonka.Nimble.Types.StudioComponentType
import qualified Amazonka.Prelude as Prelude

-- | A network that is used by a studio’s users and workflows, including
-- render farm, Active Directory, licensing, and file system.
--
-- /See:/ 'newStudioComponent' smart constructor.
data StudioComponent = StudioComponent'
  { -- | Initialization scripts for studio components.
    initializationScripts :: Prelude.Maybe [StudioComponentInitializationScript],
    -- | The current state.
    state :: Prelude.Maybe StudioComponentState,
    -- | The unique identifier for a studio component resource.
    studioComponentId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The user ID of the user that created the studio component.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | A friendly name for the studio component resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status message for the studio component.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Parameters for the studio component scripts.
    scriptParameters :: Prelude.Maybe [ScriptParameterKeyValue],
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The type of the studio component.
    type' :: Prelude.Maybe StudioComponentType,
    -- | The configuration of the studio component, based on component type.
    configuration :: Prelude.Maybe StudioComponentConfiguration,
    -- | A human-readable description for the studio component resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The status code.
    statusCode :: Prelude.Maybe StudioComponentStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initializationScripts', 'studioComponent_initializationScripts' - Initialization scripts for studio components.
--
-- 'state', 'studioComponent_state' - The current state.
--
-- 'studioComponentId', 'studioComponent_studioComponentId' - The unique identifier for a studio component resource.
--
-- 'arn', 'studioComponent_arn' - The ARN of the resource.
--
-- 'createdAt', 'studioComponent_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'createdBy', 'studioComponent_createdBy' - The user ID of the user that created the studio component.
--
-- 'ec2SecurityGroupIds', 'studioComponent_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'updatedBy', 'studioComponent_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'subtype', 'studioComponent_subtype' - The specific subtype of a studio component.
--
-- 'name', 'studioComponent_name' - A friendly name for the studio component resource.
--
-- 'statusMessage', 'studioComponent_statusMessage' - The status message for the studio component.
--
-- 'scriptParameters', 'studioComponent_scriptParameters' - Parameters for the studio component scripts.
--
-- 'updatedAt', 'studioComponent_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
--
-- 'type'', 'studioComponent_type' - The type of the studio component.
--
-- 'configuration', 'studioComponent_configuration' - The configuration of the studio component, based on component type.
--
-- 'description', 'studioComponent_description' - A human-readable description for the studio component resource.
--
-- 'tags', 'studioComponent_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'statusCode', 'studioComponent_statusCode' - The status code.
newStudioComponent ::
  StudioComponent
newStudioComponent =
  StudioComponent'
    { initializationScripts =
        Prelude.Nothing,
      state = Prelude.Nothing,
      studioComponentId = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      ec2SecurityGroupIds = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      subtype = Prelude.Nothing,
      name = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      scriptParameters = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      type' = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Initialization scripts for studio components.
studioComponent_initializationScripts :: Lens.Lens' StudioComponent (Prelude.Maybe [StudioComponentInitializationScript])
studioComponent_initializationScripts = Lens.lens (\StudioComponent' {initializationScripts} -> initializationScripts) (\s@StudioComponent' {} a -> s {initializationScripts = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The current state.
studioComponent_state :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentState)
studioComponent_state = Lens.lens (\StudioComponent' {state} -> state) (\s@StudioComponent' {} a -> s {state = a} :: StudioComponent)

-- | The unique identifier for a studio component resource.
studioComponent_studioComponentId :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_studioComponentId = Lens.lens (\StudioComponent' {studioComponentId} -> studioComponentId) (\s@StudioComponent' {} a -> s {studioComponentId = a} :: StudioComponent)

-- | The ARN of the resource.
studioComponent_arn :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_arn = Lens.lens (\StudioComponent' {arn} -> arn) (\s@StudioComponent' {} a -> s {arn = a} :: StudioComponent)

-- | The Unix epoch timestamp in seconds for when the resource was created.
studioComponent_createdAt :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.UTCTime)
studioComponent_createdAt = Lens.lens (\StudioComponent' {createdAt} -> createdAt) (\s@StudioComponent' {} a -> s {createdAt = a} :: StudioComponent) Prelude.. Lens.mapping Core._Time

-- | The user ID of the user that created the studio component.
studioComponent_createdBy :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_createdBy = Lens.lens (\StudioComponent' {createdBy} -> createdBy) (\s@StudioComponent' {} a -> s {createdBy = a} :: StudioComponent)

-- | The EC2 security groups that control access to the studio component.
studioComponent_ec2SecurityGroupIds :: Lens.Lens' StudioComponent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
studioComponent_ec2SecurityGroupIds = Lens.lens (\StudioComponent' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@StudioComponent' {} a -> s {ec2SecurityGroupIds = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The user ID of the user that most recently updated the resource.
studioComponent_updatedBy :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_updatedBy = Lens.lens (\StudioComponent' {updatedBy} -> updatedBy) (\s@StudioComponent' {} a -> s {updatedBy = a} :: StudioComponent)

-- | The specific subtype of a studio component.
studioComponent_subtype :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentSubtype)
studioComponent_subtype = Lens.lens (\StudioComponent' {subtype} -> subtype) (\s@StudioComponent' {} a -> s {subtype = a} :: StudioComponent)

-- | A friendly name for the studio component resource.
studioComponent_name :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_name = Lens.lens (\StudioComponent' {name} -> name) (\s@StudioComponent' {} a -> s {name = a} :: StudioComponent)

-- | The status message for the studio component.
studioComponent_statusMessage :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_statusMessage = Lens.lens (\StudioComponent' {statusMessage} -> statusMessage) (\s@StudioComponent' {} a -> s {statusMessage = a} :: StudioComponent)

-- | Parameters for the studio component scripts.
studioComponent_scriptParameters :: Lens.Lens' StudioComponent (Prelude.Maybe [ScriptParameterKeyValue])
studioComponent_scriptParameters = Lens.lens (\StudioComponent' {scriptParameters} -> scriptParameters) (\s@StudioComponent' {} a -> s {scriptParameters = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The Unix epoch timestamp in seconds for when the resource was updated.
studioComponent_updatedAt :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.UTCTime)
studioComponent_updatedAt = Lens.lens (\StudioComponent' {updatedAt} -> updatedAt) (\s@StudioComponent' {} a -> s {updatedAt = a} :: StudioComponent) Prelude.. Lens.mapping Core._Time

-- | The type of the studio component.
studioComponent_type :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentType)
studioComponent_type = Lens.lens (\StudioComponent' {type'} -> type') (\s@StudioComponent' {} a -> s {type' = a} :: StudioComponent)

-- | The configuration of the studio component, based on component type.
studioComponent_configuration :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentConfiguration)
studioComponent_configuration = Lens.lens (\StudioComponent' {configuration} -> configuration) (\s@StudioComponent' {} a -> s {configuration = a} :: StudioComponent)

-- | A human-readable description for the studio component resource.
studioComponent_description :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_description = Lens.lens (\StudioComponent' {description} -> description) (\s@StudioComponent' {} a -> s {description = a} :: StudioComponent)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
studioComponent_tags :: Lens.Lens' StudioComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
studioComponent_tags = Lens.lens (\StudioComponent' {tags} -> tags) (\s@StudioComponent' {} a -> s {tags = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The status code.
studioComponent_statusCode :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentStatusCode)
studioComponent_statusCode = Lens.lens (\StudioComponent' {statusCode} -> statusCode) (\s@StudioComponent' {} a -> s {statusCode = a} :: StudioComponent)

instance Core.FromJSON StudioComponent where
  parseJSON =
    Core.withObject
      "StudioComponent"
      ( \x ->
          StudioComponent'
            Prelude.<$> ( x Core..:? "initializationScripts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "studioComponentId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "ec2SecurityGroupIds")
            Prelude.<*> (x Core..:? "updatedBy")
            Prelude.<*> (x Core..:? "subtype")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> ( x Core..:? "scriptParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "configuration")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "statusCode")
      )

instance Prelude.Hashable StudioComponent where
  hashWithSalt _salt StudioComponent' {..} =
    _salt `Prelude.hashWithSalt` initializationScripts
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` studioComponentId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` subtype
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` scriptParameters
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData StudioComponent where
  rnf StudioComponent' {..} =
    Prelude.rnf initializationScripts
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf ec2SecurityGroupIds
      `Prelude.seq` Prelude.rnf updatedBy
      `Prelude.seq` Prelude.rnf subtype
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf scriptParameters
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf statusCode
