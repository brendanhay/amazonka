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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.ScriptParameterKeyValue
import Amazonka.Nimble.Types.StudioComponentConfiguration
import Amazonka.Nimble.Types.StudioComponentInitializationScript
import Amazonka.Nimble.Types.StudioComponentState
import Amazonka.Nimble.Types.StudioComponentStatusCode
import Amazonka.Nimble.Types.StudioComponentSubtype
import Amazonka.Nimble.Types.StudioComponentType
import qualified Amazonka.Prelude as Prelude

-- | A studio component represents a network resource to be used by a
-- studio\'s users and workflows. A typical studio contains studio
-- components for each of the following: render farm, Active Directory,
-- licensing, and file system.
--
-- Access to a studio component is managed by specifying security groups
-- for the resource, as well as its endpoint.
--
-- A studio component also has a set of initialization scripts that are
-- returned by @GetLaunchProfileInitialization@. These initialization
-- scripts run on streaming sessions when they start. They provide users
-- with flexibility in controlling how the studio resources are configured
-- on a streaming session.
--
-- /See:/ 'newStudioComponent' smart constructor.
data StudioComponent = StudioComponent'
  { -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Parameters for the studio component scripts.
    scriptParameters :: Prelude.Maybe (Data.Sensitive [ScriptParameterKeyValue]),
    -- | A friendly name for the studio component resource.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The type of the studio component.
    type' :: Prelude.Maybe StudioComponentType,
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | Initialization scripts for studio components.
    initializationScripts :: Prelude.Maybe [StudioComponentInitializationScript],
    -- | The configuration of the studio component, based on component type.
    configuration :: Prelude.Maybe StudioComponentConfiguration,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state.
    state :: Prelude.Maybe StudioComponentState,
    -- | A human-readable description for the studio component resource.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An IAM role attached to Studio Component when the system initialization
    -- script runs which give the studio component access to AWS resources when
    -- the system initialization script runs.
    secureInitializationRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a studio component resource.
    studioComponentId :: Prelude.Maybe Prelude.Text,
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | An IAM role attached to a Studio Component that gives the studio
    -- component access to AWS resources at anytime while the instance is
    -- running.
    runtimeRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The status code.
    statusCode :: Prelude.Maybe StudioComponentStatusCode,
    -- | The user ID of the user that created the studio component.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The status message for the studio component.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'studioComponent_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'scriptParameters', 'studioComponent_scriptParameters' - Parameters for the studio component scripts.
--
-- 'name', 'studioComponent_name' - A friendly name for the studio component resource.
--
-- 'type'', 'studioComponent_type' - The type of the studio component.
--
-- 'updatedBy', 'studioComponent_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'initializationScripts', 'studioComponent_initializationScripts' - Initialization scripts for studio components.
--
-- 'configuration', 'studioComponent_configuration' - The configuration of the studio component, based on component type.
--
-- 'arn', 'studioComponent_arn' - The ARN of the resource.
--
-- 'state', 'studioComponent_state' - The current state.
--
-- 'description', 'studioComponent_description' - A human-readable description for the studio component resource.
--
-- 'secureInitializationRoleArn', 'studioComponent_secureInitializationRoleArn' - An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to AWS resources when
-- the system initialization script runs.
--
-- 'studioComponentId', 'studioComponent_studioComponentId' - The unique identifier for a studio component resource.
--
-- 'subtype', 'studioComponent_subtype' - The specific subtype of a studio component.
--
-- 'runtimeRoleArn', 'studioComponent_runtimeRoleArn' - An IAM role attached to a Studio Component that gives the studio
-- component access to AWS resources at anytime while the instance is
-- running.
--
-- 'statusCode', 'studioComponent_statusCode' - The status code.
--
-- 'createdBy', 'studioComponent_createdBy' - The user ID of the user that created the studio component.
--
-- 'statusMessage', 'studioComponent_statusMessage' - The status message for the studio component.
--
-- 'ec2SecurityGroupIds', 'studioComponent_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'createdAt', 'studioComponent_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'updatedAt', 'studioComponent_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
newStudioComponent ::
  StudioComponent
newStudioComponent =
  StudioComponent'
    { tags = Prelude.Nothing,
      scriptParameters = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      initializationScripts = Prelude.Nothing,
      configuration = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      secureInitializationRoleArn = Prelude.Nothing,
      studioComponentId = Prelude.Nothing,
      subtype = Prelude.Nothing,
      runtimeRoleArn = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      ec2SecurityGroupIds = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
studioComponent_tags :: Lens.Lens' StudioComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
studioComponent_tags = Lens.lens (\StudioComponent' {tags} -> tags) (\s@StudioComponent' {} a -> s {tags = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | Parameters for the studio component scripts.
studioComponent_scriptParameters :: Lens.Lens' StudioComponent (Prelude.Maybe [ScriptParameterKeyValue])
studioComponent_scriptParameters = Lens.lens (\StudioComponent' {scriptParameters} -> scriptParameters) (\s@StudioComponent' {} a -> s {scriptParameters = a} :: StudioComponent) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A friendly name for the studio component resource.
studioComponent_name :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_name = Lens.lens (\StudioComponent' {name} -> name) (\s@StudioComponent' {} a -> s {name = a} :: StudioComponent) Prelude.. Lens.mapping Data._Sensitive

-- | The type of the studio component.
studioComponent_type :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentType)
studioComponent_type = Lens.lens (\StudioComponent' {type'} -> type') (\s@StudioComponent' {} a -> s {type' = a} :: StudioComponent)

-- | The user ID of the user that most recently updated the resource.
studioComponent_updatedBy :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_updatedBy = Lens.lens (\StudioComponent' {updatedBy} -> updatedBy) (\s@StudioComponent' {} a -> s {updatedBy = a} :: StudioComponent)

-- | Initialization scripts for studio components.
studioComponent_initializationScripts :: Lens.Lens' StudioComponent (Prelude.Maybe [StudioComponentInitializationScript])
studioComponent_initializationScripts = Lens.lens (\StudioComponent' {initializationScripts} -> initializationScripts) (\s@StudioComponent' {} a -> s {initializationScripts = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of the studio component, based on component type.
studioComponent_configuration :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentConfiguration)
studioComponent_configuration = Lens.lens (\StudioComponent' {configuration} -> configuration) (\s@StudioComponent' {} a -> s {configuration = a} :: StudioComponent)

-- | The ARN of the resource.
studioComponent_arn :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_arn = Lens.lens (\StudioComponent' {arn} -> arn) (\s@StudioComponent' {} a -> s {arn = a} :: StudioComponent)

-- | The current state.
studioComponent_state :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentState)
studioComponent_state = Lens.lens (\StudioComponent' {state} -> state) (\s@StudioComponent' {} a -> s {state = a} :: StudioComponent)

-- | A human-readable description for the studio component resource.
studioComponent_description :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_description = Lens.lens (\StudioComponent' {description} -> description) (\s@StudioComponent' {} a -> s {description = a} :: StudioComponent) Prelude.. Lens.mapping Data._Sensitive

-- | An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to AWS resources when
-- the system initialization script runs.
studioComponent_secureInitializationRoleArn :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_secureInitializationRoleArn = Lens.lens (\StudioComponent' {secureInitializationRoleArn} -> secureInitializationRoleArn) (\s@StudioComponent' {} a -> s {secureInitializationRoleArn = a} :: StudioComponent)

-- | The unique identifier for a studio component resource.
studioComponent_studioComponentId :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_studioComponentId = Lens.lens (\StudioComponent' {studioComponentId} -> studioComponentId) (\s@StudioComponent' {} a -> s {studioComponentId = a} :: StudioComponent)

-- | The specific subtype of a studio component.
studioComponent_subtype :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentSubtype)
studioComponent_subtype = Lens.lens (\StudioComponent' {subtype} -> subtype) (\s@StudioComponent' {} a -> s {subtype = a} :: StudioComponent)

-- | An IAM role attached to a Studio Component that gives the studio
-- component access to AWS resources at anytime while the instance is
-- running.
studioComponent_runtimeRoleArn :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_runtimeRoleArn = Lens.lens (\StudioComponent' {runtimeRoleArn} -> runtimeRoleArn) (\s@StudioComponent' {} a -> s {runtimeRoleArn = a} :: StudioComponent)

-- | The status code.
studioComponent_statusCode :: Lens.Lens' StudioComponent (Prelude.Maybe StudioComponentStatusCode)
studioComponent_statusCode = Lens.lens (\StudioComponent' {statusCode} -> statusCode) (\s@StudioComponent' {} a -> s {statusCode = a} :: StudioComponent)

-- | The user ID of the user that created the studio component.
studioComponent_createdBy :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_createdBy = Lens.lens (\StudioComponent' {createdBy} -> createdBy) (\s@StudioComponent' {} a -> s {createdBy = a} :: StudioComponent)

-- | The status message for the studio component.
studioComponent_statusMessage :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.Text)
studioComponent_statusMessage = Lens.lens (\StudioComponent' {statusMessage} -> statusMessage) (\s@StudioComponent' {} a -> s {statusMessage = a} :: StudioComponent)

-- | The EC2 security groups that control access to the studio component.
studioComponent_ec2SecurityGroupIds :: Lens.Lens' StudioComponent (Prelude.Maybe [Prelude.Text])
studioComponent_ec2SecurityGroupIds = Lens.lens (\StudioComponent' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@StudioComponent' {} a -> s {ec2SecurityGroupIds = a} :: StudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The Unix epoch timestamp in seconds for when the resource was created.
studioComponent_createdAt :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.UTCTime)
studioComponent_createdAt = Lens.lens (\StudioComponent' {createdAt} -> createdAt) (\s@StudioComponent' {} a -> s {createdAt = a} :: StudioComponent) Prelude.. Lens.mapping Data._Time

-- | The Unix epoch timestamp in seconds for when the resource was updated.
studioComponent_updatedAt :: Lens.Lens' StudioComponent (Prelude.Maybe Prelude.UTCTime)
studioComponent_updatedAt = Lens.lens (\StudioComponent' {updatedAt} -> updatedAt) (\s@StudioComponent' {} a -> s {updatedAt = a} :: StudioComponent) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON StudioComponent where
  parseJSON =
    Data.withObject
      "StudioComponent"
      ( \x ->
          StudioComponent'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "scriptParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "updatedBy")
            Prelude.<*> ( x Data..:? "initializationScripts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "configuration")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "secureInitializationRoleArn")
            Prelude.<*> (x Data..:? "studioComponentId")
            Prelude.<*> (x Data..:? "subtype")
            Prelude.<*> (x Data..:? "runtimeRoleArn")
            Prelude.<*> (x Data..:? "statusCode")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> ( x Data..:? "ec2SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable StudioComponent where
  hashWithSalt _salt StudioComponent' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` scriptParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` initializationScripts
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` secureInitializationRoleArn
      `Prelude.hashWithSalt` studioComponentId
      `Prelude.hashWithSalt` subtype
      `Prelude.hashWithSalt` runtimeRoleArn
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData StudioComponent where
  rnf StudioComponent' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf scriptParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedBy
      `Prelude.seq` Prelude.rnf initializationScripts
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf secureInitializationRoleArn
      `Prelude.seq` Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf subtype
      `Prelude.seq` Prelude.rnf runtimeRoleArn
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf ec2SecurityGroupIds
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
