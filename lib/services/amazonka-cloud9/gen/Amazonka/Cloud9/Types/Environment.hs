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
-- Module      : Amazonka.Cloud9.Types.Environment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.Environment where

import Amazonka.Cloud9.Types.ConnectionType
import Amazonka.Cloud9.Types.EnvironmentLifecycle
import Amazonka.Cloud9.Types.EnvironmentType
import Amazonka.Cloud9.Types.ManagedCredentialsStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Cloud9 development environment.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The connection type used for connecting to an Amazon EC2 environment.
    -- @CONNECT_SSH@ is selected by default.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | The description for the environment.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the environment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the environment in its creation or deletion lifecycle.
    lifecycle :: Prelude.Maybe EnvironmentLifecycle,
    -- | Describes the status of Amazon Web Services managed temporary
    -- credentials for the Cloud9 environment. Available values are:
    --
    -- -   @ENABLED_ON_CREATE@
    --
    -- -   @ENABLED_BY_OWNER@
    --
    -- -   @DISABLED_BY_DEFAULT@
    --
    -- -   @DISABLED_BY_OWNER@
    --
    -- -   @DISABLED_BY_COLLABORATOR@
    --
    -- -   @PENDING_REMOVAL_BY_COLLABORATOR@
    --
    -- -   @PENDING_REMOVAL_BY_OWNER@
    --
    -- -   @FAILED_REMOVAL_BY_COLLABORATOR@
    --
    -- -   @ENABLED_BY_OWNER@
    --
    -- -   @DISABLED_BY_DEFAULT@
    managedCredentialsStatus :: Prelude.Maybe ManagedCredentialsStatus,
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of environment. Valid values include the following:
    --
    -- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
    --     connects to the environment.
    --
    -- -   @ssh@: Your own server connects to the environment.
    type' :: EnvironmentType,
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment owner.
    ownerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionType', 'environment_connectionType' - The connection type used for connecting to an Amazon EC2 environment.
-- @CONNECT_SSH@ is selected by default.
--
-- 'description', 'environment_description' - The description for the environment.
--
-- 'id', 'environment_id' - The ID of the environment.
--
-- 'lifecycle', 'environment_lifecycle' - The state of the environment in its creation or deletion lifecycle.
--
-- 'managedCredentialsStatus', 'environment_managedCredentialsStatus' - Describes the status of Amazon Web Services managed temporary
-- credentials for the Cloud9 environment. Available values are:
--
-- -   @ENABLED_ON_CREATE@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
--
-- -   @DISABLED_BY_OWNER@
--
-- -   @DISABLED_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_OWNER@
--
-- -   @FAILED_REMOVAL_BY_COLLABORATOR@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'type'', 'environment_type' - The type of environment. Valid values include the following:
--
-- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
--     connects to the environment.
--
-- -   @ssh@: Your own server connects to the environment.
--
-- 'arn', 'environment_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'ownerArn', 'environment_ownerArn' - The Amazon Resource Name (ARN) of the environment owner.
newEnvironment ::
  -- | 'type''
  EnvironmentType ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'ownerArn'
  Prelude.Text ->
  Environment
newEnvironment pType_ pArn_ pOwnerArn_ =
  Environment'
    { connectionType = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      managedCredentialsStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = pType_,
      arn = pArn_,
      ownerArn = pOwnerArn_
    }

-- | The connection type used for connecting to an Amazon EC2 environment.
-- @CONNECT_SSH@ is selected by default.
environment_connectionType :: Lens.Lens' Environment (Prelude.Maybe ConnectionType)
environment_connectionType = Lens.lens (\Environment' {connectionType} -> connectionType) (\s@Environment' {} a -> s {connectionType = a} :: Environment)

-- | The description for the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the environment.
environment_id :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_id = Lens.lens (\Environment' {id} -> id) (\s@Environment' {} a -> s {id = a} :: Environment)

-- | The state of the environment in its creation or deletion lifecycle.
environment_lifecycle :: Lens.Lens' Environment (Prelude.Maybe EnvironmentLifecycle)
environment_lifecycle = Lens.lens (\Environment' {lifecycle} -> lifecycle) (\s@Environment' {} a -> s {lifecycle = a} :: Environment)

-- | Describes the status of Amazon Web Services managed temporary
-- credentials for the Cloud9 environment. Available values are:
--
-- -   @ENABLED_ON_CREATE@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
--
-- -   @DISABLED_BY_OWNER@
--
-- -   @DISABLED_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_OWNER@
--
-- -   @FAILED_REMOVAL_BY_COLLABORATOR@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
environment_managedCredentialsStatus :: Lens.Lens' Environment (Prelude.Maybe ManagedCredentialsStatus)
environment_managedCredentialsStatus = Lens.lens (\Environment' {managedCredentialsStatus} -> managedCredentialsStatus) (\s@Environment' {} a -> s {managedCredentialsStatus = a} :: Environment)

-- | The name of the environment.
environment_name :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The type of environment. Valid values include the following:
--
-- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
--     connects to the environment.
--
-- -   @ssh@: Your own server connects to the environment.
environment_type :: Lens.Lens' Environment EnvironmentType
environment_type = Lens.lens (\Environment' {type'} -> type') (\s@Environment' {} a -> s {type' = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment.
environment_arn :: Lens.Lens' Environment Prelude.Text
environment_arn = Lens.lens (\Environment' {arn} -> arn) (\s@Environment' {} a -> s {arn = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment owner.
environment_ownerArn :: Lens.Lens' Environment Prelude.Text
environment_ownerArn = Lens.lens (\Environment' {ownerArn} -> ownerArn) (\s@Environment' {} a -> s {ownerArn = a} :: Environment)

instance Data.FromJSON Environment where
  parseJSON =
    Data.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Data..:? "connectionType")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lifecycle")
            Prelude.<*> (x Data..:? "managedCredentialsStatus")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "ownerArn")
      )

instance Prelude.Hashable Environment where
  hashWithSalt _salt Environment' {..} =
    _salt
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` managedCredentialsStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` ownerArn

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf managedCredentialsStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf ownerArn
