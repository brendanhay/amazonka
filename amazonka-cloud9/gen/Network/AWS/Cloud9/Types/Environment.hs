{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Cloud9.Types.Environment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.Environment where

import Network.AWS.Cloud9.Types.ConnectionType
import Network.AWS.Cloud9.Types.EnvironmentLifecycle
import Network.AWS.Cloud9.Types.EnvironmentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an AWS Cloud9 development environment.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The state of the environment in its creation or deletion lifecycle.
    lifecycle :: Prelude.Maybe EnvironmentLifecycle,
    -- | The connection type used for connecting to an Amazon EC2 environment.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | The ID of the environment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment owner.
    ownerArn :: Prelude.Maybe Prelude.Text,
    -- | The description for the environment.
    description :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The type of environment. Valid values include the following:
    --
    -- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
    --     connects to the environment.
    --
    -- -   @ssh@: Your own server connects to the environment.
    type' :: Prelude.Maybe EnvironmentType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'environment_lifecycle' - The state of the environment in its creation or deletion lifecycle.
--
-- 'connectionType', 'environment_connectionType' - The connection type used for connecting to an Amazon EC2 environment.
--
-- 'id', 'environment_id' - The ID of the environment.
--
-- 'arn', 'environment_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'ownerArn', 'environment_ownerArn' - The Amazon Resource Name (ARN) of the environment owner.
--
-- 'description', 'environment_description' - The description for the environment.
--
-- 'type'', 'environment_type' - The type of environment. Valid values include the following:
--
-- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
--     connects to the environment.
--
-- -   @ssh@: Your own server connects to the environment.
newEnvironment ::
  Environment
newEnvironment =
  Environment'
    { lifecycle = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      id = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerArn = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The state of the environment in its creation or deletion lifecycle.
environment_lifecycle :: Lens.Lens' Environment (Prelude.Maybe EnvironmentLifecycle)
environment_lifecycle = Lens.lens (\Environment' {lifecycle} -> lifecycle) (\s@Environment' {} a -> s {lifecycle = a} :: Environment)

-- | The connection type used for connecting to an Amazon EC2 environment.
environment_connectionType :: Lens.Lens' Environment (Prelude.Maybe ConnectionType)
environment_connectionType = Lens.lens (\Environment' {connectionType} -> connectionType) (\s@Environment' {} a -> s {connectionType = a} :: Environment)

-- | The ID of the environment.
environment_id :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_id = Lens.lens (\Environment' {id} -> id) (\s@Environment' {} a -> s {id = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment.
environment_arn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_arn = Lens.lens (\Environment' {arn} -> arn) (\s@Environment' {} a -> s {arn = a} :: Environment)

-- | The name of the environment.
environment_name :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment owner.
environment_ownerArn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_ownerArn = Lens.lens (\Environment' {ownerArn} -> ownerArn) (\s@Environment' {} a -> s {ownerArn = a} :: Environment)

-- | The description for the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment) Prelude.. Lens.mapping Prelude._Sensitive

-- | The type of environment. Valid values include the following:
--
-- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
--     connects to the environment.
--
-- -   @ssh@: Your own server connects to the environment.
environment_type :: Lens.Lens' Environment (Prelude.Maybe EnvironmentType)
environment_type = Lens.lens (\Environment' {type'} -> type') (\s@Environment' {} a -> s {type' = a} :: Environment)

instance Prelude.FromJSON Environment where
  parseJSON =
    Prelude.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Prelude..:? "lifecycle")
            Prelude.<*> (x Prelude..:? "connectionType")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "ownerArn")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable Environment

instance Prelude.NFData Environment
