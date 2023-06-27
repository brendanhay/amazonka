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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceBundle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceBundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.BundleType
import Amazonka.WorkSpaces.Types.ComputeType
import Amazonka.WorkSpaces.Types.RootStorage
import Amazonka.WorkSpaces.Types.UserStorage
import Amazonka.WorkSpaces.Types.WorkspaceBundleState

-- | Describes a WorkSpace bundle.
--
-- /See:/ 'newWorkspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { -- | The identifier of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The type of WorkSpace bundle.
    bundleType :: Prelude.Maybe BundleType,
    -- | The compute type of the bundle. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeType :: Prelude.Maybe ComputeType,
    -- | The time when the bundle was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the bundle.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the image that was used to create the bundle.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The last time that the bundle was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the bundle. This is the account identifier of the owner, or
    -- @AMAZON@ if the bundle is provided by Amazon Web Services.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The size of the root volume.
    rootStorage :: Prelude.Maybe RootStorage,
    -- | The state of the WorkSpace bundle.
    state :: Prelude.Maybe WorkspaceBundleState,
    -- | The size of the user volume.
    userStorage :: Prelude.Maybe UserStorage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'workspaceBundle_bundleId' - The identifier of the bundle.
--
-- 'bundleType', 'workspaceBundle_bundleType' - The type of WorkSpace bundle.
--
-- 'computeType', 'workspaceBundle_computeType' - The compute type of the bundle. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
--
-- 'creationTime', 'workspaceBundle_creationTime' - The time when the bundle was created.
--
-- 'description', 'workspaceBundle_description' - The description of the bundle.
--
-- 'imageId', 'workspaceBundle_imageId' - The identifier of the image that was used to create the bundle.
--
-- 'lastUpdatedTime', 'workspaceBundle_lastUpdatedTime' - The last time that the bundle was updated.
--
-- 'name', 'workspaceBundle_name' - The name of the bundle.
--
-- 'owner', 'workspaceBundle_owner' - The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by Amazon Web Services.
--
-- 'rootStorage', 'workspaceBundle_rootStorage' - The size of the root volume.
--
-- 'state', 'workspaceBundle_state' - The state of the WorkSpace bundle.
--
-- 'userStorage', 'workspaceBundle_userStorage' - The size of the user volume.
newWorkspaceBundle ::
  WorkspaceBundle
newWorkspaceBundle =
  WorkspaceBundle'
    { bundleId = Prelude.Nothing,
      bundleType = Prelude.Nothing,
      computeType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      imageId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      rootStorage = Prelude.Nothing,
      state = Prelude.Nothing,
      userStorage = Prelude.Nothing
    }

-- | The identifier of the bundle.
workspaceBundle_bundleId :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_bundleId = Lens.lens (\WorkspaceBundle' {bundleId} -> bundleId) (\s@WorkspaceBundle' {} a -> s {bundleId = a} :: WorkspaceBundle)

-- | The type of WorkSpace bundle.
workspaceBundle_bundleType :: Lens.Lens' WorkspaceBundle (Prelude.Maybe BundleType)
workspaceBundle_bundleType = Lens.lens (\WorkspaceBundle' {bundleType} -> bundleType) (\s@WorkspaceBundle' {} a -> s {bundleType = a} :: WorkspaceBundle)

-- | The compute type of the bundle. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceBundle_computeType :: Lens.Lens' WorkspaceBundle (Prelude.Maybe ComputeType)
workspaceBundle_computeType = Lens.lens (\WorkspaceBundle' {computeType} -> computeType) (\s@WorkspaceBundle' {} a -> s {computeType = a} :: WorkspaceBundle)

-- | The time when the bundle was created.
workspaceBundle_creationTime :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.UTCTime)
workspaceBundle_creationTime = Lens.lens (\WorkspaceBundle' {creationTime} -> creationTime) (\s@WorkspaceBundle' {} a -> s {creationTime = a} :: WorkspaceBundle) Prelude.. Lens.mapping Data._Time

-- | The description of the bundle.
workspaceBundle_description :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_description = Lens.lens (\WorkspaceBundle' {description} -> description) (\s@WorkspaceBundle' {} a -> s {description = a} :: WorkspaceBundle)

-- | The identifier of the image that was used to create the bundle.
workspaceBundle_imageId :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_imageId = Lens.lens (\WorkspaceBundle' {imageId} -> imageId) (\s@WorkspaceBundle' {} a -> s {imageId = a} :: WorkspaceBundle)

-- | The last time that the bundle was updated.
workspaceBundle_lastUpdatedTime :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.UTCTime)
workspaceBundle_lastUpdatedTime = Lens.lens (\WorkspaceBundle' {lastUpdatedTime} -> lastUpdatedTime) (\s@WorkspaceBundle' {} a -> s {lastUpdatedTime = a} :: WorkspaceBundle) Prelude.. Lens.mapping Data._Time

-- | The name of the bundle.
workspaceBundle_name :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_name = Lens.lens (\WorkspaceBundle' {name} -> name) (\s@WorkspaceBundle' {} a -> s {name = a} :: WorkspaceBundle)

-- | The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by Amazon Web Services.
workspaceBundle_owner :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_owner = Lens.lens (\WorkspaceBundle' {owner} -> owner) (\s@WorkspaceBundle' {} a -> s {owner = a} :: WorkspaceBundle)

-- | The size of the root volume.
workspaceBundle_rootStorage :: Lens.Lens' WorkspaceBundle (Prelude.Maybe RootStorage)
workspaceBundle_rootStorage = Lens.lens (\WorkspaceBundle' {rootStorage} -> rootStorage) (\s@WorkspaceBundle' {} a -> s {rootStorage = a} :: WorkspaceBundle)

-- | The state of the WorkSpace bundle.
workspaceBundle_state :: Lens.Lens' WorkspaceBundle (Prelude.Maybe WorkspaceBundleState)
workspaceBundle_state = Lens.lens (\WorkspaceBundle' {state} -> state) (\s@WorkspaceBundle' {} a -> s {state = a} :: WorkspaceBundle)

-- | The size of the user volume.
workspaceBundle_userStorage :: Lens.Lens' WorkspaceBundle (Prelude.Maybe UserStorage)
workspaceBundle_userStorage = Lens.lens (\WorkspaceBundle' {userStorage} -> userStorage) (\s@WorkspaceBundle' {} a -> s {userStorage = a} :: WorkspaceBundle)

instance Data.FromJSON WorkspaceBundle where
  parseJSON =
    Data.withObject
      "WorkspaceBundle"
      ( \x ->
          WorkspaceBundle'
            Prelude.<$> (x Data..:? "BundleId")
            Prelude.<*> (x Data..:? "BundleType")
            Prelude.<*> (x Data..:? "ComputeType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ImageId")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "RootStorage")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "UserStorage")
      )

instance Prelude.Hashable WorkspaceBundle where
  hashWithSalt _salt WorkspaceBundle' {..} =
    _salt
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` bundleType
      `Prelude.hashWithSalt` computeType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` rootStorage
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` userStorage

instance Prelude.NFData WorkspaceBundle where
  rnf WorkspaceBundle' {..} =
    Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf bundleType
      `Prelude.seq` Prelude.rnf computeType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf rootStorage
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf userStorage
