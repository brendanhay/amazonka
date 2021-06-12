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
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.UserStorage

-- | Describes a WorkSpace bundle.
--
-- /See:/ 'newWorkspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { -- | The size of the root volume.
    rootStorage :: Core.Maybe RootStorage,
    -- | The bundle identifier.
    bundleId :: Core.Maybe Core.Text,
    -- | The size of the user storage.
    userStorage :: Core.Maybe UserStorage,
    -- | The image identifier of the bundle.
    imageId :: Core.Maybe Core.Text,
    -- | The name of the bundle.
    name :: Core.Maybe Core.Text,
    -- | The owner of the bundle. This is the account identifier of the owner, or
    -- @AMAZON@ if the bundle is provided by AWS.
    owner :: Core.Maybe Core.Text,
    -- | A description.
    description :: Core.Maybe Core.Text,
    -- | The compute type. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeType :: Core.Maybe ComputeType,
    -- | The last time that the bundle was updated.
    lastUpdatedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'bundleId', 'workspaceBundle_bundleId' - The bundle identifier.
--
-- 'userStorage', 'workspaceBundle_userStorage' - The size of the user storage.
--
-- 'imageId', 'workspaceBundle_imageId' - The image identifier of the bundle.
--
-- 'name', 'workspaceBundle_name' - The name of the bundle.
--
-- 'owner', 'workspaceBundle_owner' - The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by AWS.
--
-- 'description', 'workspaceBundle_description' - A description.
--
-- 'computeType', 'workspaceBundle_computeType' - The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
--
-- 'lastUpdatedTime', 'workspaceBundle_lastUpdatedTime' - The last time that the bundle was updated.
newWorkspaceBundle ::
  WorkspaceBundle
newWorkspaceBundle =
  WorkspaceBundle'
    { rootStorage = Core.Nothing,
      bundleId = Core.Nothing,
      userStorage = Core.Nothing,
      imageId = Core.Nothing,
      name = Core.Nothing,
      owner = Core.Nothing,
      description = Core.Nothing,
      computeType = Core.Nothing,
      lastUpdatedTime = Core.Nothing
    }

-- | The size of the root volume.
workspaceBundle_rootStorage :: Lens.Lens' WorkspaceBundle (Core.Maybe RootStorage)
workspaceBundle_rootStorage = Lens.lens (\WorkspaceBundle' {rootStorage} -> rootStorage) (\s@WorkspaceBundle' {} a -> s {rootStorage = a} :: WorkspaceBundle)

-- | The bundle identifier.
workspaceBundle_bundleId :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.Text)
workspaceBundle_bundleId = Lens.lens (\WorkspaceBundle' {bundleId} -> bundleId) (\s@WorkspaceBundle' {} a -> s {bundleId = a} :: WorkspaceBundle)

-- | The size of the user storage.
workspaceBundle_userStorage :: Lens.Lens' WorkspaceBundle (Core.Maybe UserStorage)
workspaceBundle_userStorage = Lens.lens (\WorkspaceBundle' {userStorage} -> userStorage) (\s@WorkspaceBundle' {} a -> s {userStorage = a} :: WorkspaceBundle)

-- | The image identifier of the bundle.
workspaceBundle_imageId :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.Text)
workspaceBundle_imageId = Lens.lens (\WorkspaceBundle' {imageId} -> imageId) (\s@WorkspaceBundle' {} a -> s {imageId = a} :: WorkspaceBundle)

-- | The name of the bundle.
workspaceBundle_name :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.Text)
workspaceBundle_name = Lens.lens (\WorkspaceBundle' {name} -> name) (\s@WorkspaceBundle' {} a -> s {name = a} :: WorkspaceBundle)

-- | The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by AWS.
workspaceBundle_owner :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.Text)
workspaceBundle_owner = Lens.lens (\WorkspaceBundle' {owner} -> owner) (\s@WorkspaceBundle' {} a -> s {owner = a} :: WorkspaceBundle)

-- | A description.
workspaceBundle_description :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.Text)
workspaceBundle_description = Lens.lens (\WorkspaceBundle' {description} -> description) (\s@WorkspaceBundle' {} a -> s {description = a} :: WorkspaceBundle)

-- | The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceBundle_computeType :: Lens.Lens' WorkspaceBundle (Core.Maybe ComputeType)
workspaceBundle_computeType = Lens.lens (\WorkspaceBundle' {computeType} -> computeType) (\s@WorkspaceBundle' {} a -> s {computeType = a} :: WorkspaceBundle)

-- | The last time that the bundle was updated.
workspaceBundle_lastUpdatedTime :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.UTCTime)
workspaceBundle_lastUpdatedTime = Lens.lens (\WorkspaceBundle' {lastUpdatedTime} -> lastUpdatedTime) (\s@WorkspaceBundle' {} a -> s {lastUpdatedTime = a} :: WorkspaceBundle) Core.. Lens.mapping Core._Time

instance Core.FromJSON WorkspaceBundle where
  parseJSON =
    Core.withObject
      "WorkspaceBundle"
      ( \x ->
          WorkspaceBundle'
            Core.<$> (x Core..:? "RootStorage")
            Core.<*> (x Core..:? "BundleId")
            Core.<*> (x Core..:? "UserStorage")
            Core.<*> (x Core..:? "ImageId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Owner")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "ComputeType")
            Core.<*> (x Core..:? "LastUpdatedTime")
      )

instance Core.Hashable WorkspaceBundle

instance Core.NFData WorkspaceBundle
