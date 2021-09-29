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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceBundle where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.UserStorage

-- | Describes a WorkSpace bundle.
--
-- /See:/ 'newWorkspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { -- | The size of the root volume.
    rootStorage :: Prelude.Maybe RootStorage,
    -- | The time when the bundle was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The size of the user volume.
    userStorage :: Prelude.Maybe UserStorage,
    -- | The identifier of the image that was used to create the bundle.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The name of the bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the bundle. This is the account identifier of the owner, or
    -- @AMAZON@ if the bundle is provided by AWS.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The description of the bundle.
    description :: Prelude.Maybe Prelude.Text,
    -- | The compute type of the bundle. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeType :: Prelude.Maybe ComputeType,
    -- | The last time that the bundle was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX
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
-- 'rootStorage', 'workspaceBundle_rootStorage' - The size of the root volume.
--
-- 'creationTime', 'workspaceBundle_creationTime' - The time when the bundle was created.
--
-- 'bundleId', 'workspaceBundle_bundleId' - The identifier of the bundle.
--
-- 'userStorage', 'workspaceBundle_userStorage' - The size of the user volume.
--
-- 'imageId', 'workspaceBundle_imageId' - The identifier of the image that was used to create the bundle.
--
-- 'name', 'workspaceBundle_name' - The name of the bundle.
--
-- 'owner', 'workspaceBundle_owner' - The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by AWS.
--
-- 'description', 'workspaceBundle_description' - The description of the bundle.
--
-- 'computeType', 'workspaceBundle_computeType' - The compute type of the bundle. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
--
-- 'lastUpdatedTime', 'workspaceBundle_lastUpdatedTime' - The last time that the bundle was updated.
newWorkspaceBundle ::
  WorkspaceBundle
newWorkspaceBundle =
  WorkspaceBundle'
    { rootStorage = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      userStorage = Prelude.Nothing,
      imageId = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      description = Prelude.Nothing,
      computeType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The size of the root volume.
workspaceBundle_rootStorage :: Lens.Lens' WorkspaceBundle (Prelude.Maybe RootStorage)
workspaceBundle_rootStorage = Lens.lens (\WorkspaceBundle' {rootStorage} -> rootStorage) (\s@WorkspaceBundle' {} a -> s {rootStorage = a} :: WorkspaceBundle)

-- | The time when the bundle was created.
workspaceBundle_creationTime :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.UTCTime)
workspaceBundle_creationTime = Lens.lens (\WorkspaceBundle' {creationTime} -> creationTime) (\s@WorkspaceBundle' {} a -> s {creationTime = a} :: WorkspaceBundle) Prelude.. Lens.mapping Core._Time

-- | The identifier of the bundle.
workspaceBundle_bundleId :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_bundleId = Lens.lens (\WorkspaceBundle' {bundleId} -> bundleId) (\s@WorkspaceBundle' {} a -> s {bundleId = a} :: WorkspaceBundle)

-- | The size of the user volume.
workspaceBundle_userStorage :: Lens.Lens' WorkspaceBundle (Prelude.Maybe UserStorage)
workspaceBundle_userStorage = Lens.lens (\WorkspaceBundle' {userStorage} -> userStorage) (\s@WorkspaceBundle' {} a -> s {userStorage = a} :: WorkspaceBundle)

-- | The identifier of the image that was used to create the bundle.
workspaceBundle_imageId :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_imageId = Lens.lens (\WorkspaceBundle' {imageId} -> imageId) (\s@WorkspaceBundle' {} a -> s {imageId = a} :: WorkspaceBundle)

-- | The name of the bundle.
workspaceBundle_name :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_name = Lens.lens (\WorkspaceBundle' {name} -> name) (\s@WorkspaceBundle' {} a -> s {name = a} :: WorkspaceBundle)

-- | The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by AWS.
workspaceBundle_owner :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_owner = Lens.lens (\WorkspaceBundle' {owner} -> owner) (\s@WorkspaceBundle' {} a -> s {owner = a} :: WorkspaceBundle)

-- | The description of the bundle.
workspaceBundle_description :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_description = Lens.lens (\WorkspaceBundle' {description} -> description) (\s@WorkspaceBundle' {} a -> s {description = a} :: WorkspaceBundle)

-- | The compute type of the bundle. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceBundle_computeType :: Lens.Lens' WorkspaceBundle (Prelude.Maybe ComputeType)
workspaceBundle_computeType = Lens.lens (\WorkspaceBundle' {computeType} -> computeType) (\s@WorkspaceBundle' {} a -> s {computeType = a} :: WorkspaceBundle)

-- | The last time that the bundle was updated.
workspaceBundle_lastUpdatedTime :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.UTCTime)
workspaceBundle_lastUpdatedTime = Lens.lens (\WorkspaceBundle' {lastUpdatedTime} -> lastUpdatedTime) (\s@WorkspaceBundle' {} a -> s {lastUpdatedTime = a} :: WorkspaceBundle) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON WorkspaceBundle where
  parseJSON =
    Core.withObject
      "WorkspaceBundle"
      ( \x ->
          WorkspaceBundle'
            Prelude.<$> (x Core..:? "RootStorage")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "BundleId")
            Prelude.<*> (x Core..:? "UserStorage")
            Prelude.<*> (x Core..:? "ImageId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ComputeType")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
      )

instance Prelude.Hashable WorkspaceBundle

instance Prelude.NFData WorkspaceBundle
