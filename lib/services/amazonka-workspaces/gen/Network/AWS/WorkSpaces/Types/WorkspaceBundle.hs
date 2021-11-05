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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceBundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.ComputeType
import Amazonka.WorkSpaces.Types.RootStorage
import Amazonka.WorkSpaces.Types.UserStorage

-- | Describes a WorkSpace bundle.
--
-- /See:/ 'newWorkspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { -- | The time when the bundle was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The last time that the bundle was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the bundle. This is the account identifier of the owner, or
    -- @AMAZON@ if the bundle is provided by Amazon Web Services.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The size of the root volume.
    rootStorage :: Prelude.Maybe RootStorage,
    -- | The name of the bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the image that was used to create the bundle.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The compute type of the bundle. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeType :: Prelude.Maybe ComputeType,
    -- | The size of the user volume.
    userStorage :: Prelude.Maybe UserStorage,
    -- | The description of the bundle.
    description :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'workspaceBundle_creationTime' - The time when the bundle was created.
--
-- 'lastUpdatedTime', 'workspaceBundle_lastUpdatedTime' - The last time that the bundle was updated.
--
-- 'bundleId', 'workspaceBundle_bundleId' - The identifier of the bundle.
--
-- 'owner', 'workspaceBundle_owner' - The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by Amazon Web Services.
--
-- 'rootStorage', 'workspaceBundle_rootStorage' - The size of the root volume.
--
-- 'name', 'workspaceBundle_name' - The name of the bundle.
--
-- 'imageId', 'workspaceBundle_imageId' - The identifier of the image that was used to create the bundle.
--
-- 'computeType', 'workspaceBundle_computeType' - The compute type of the bundle. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
--
-- 'userStorage', 'workspaceBundle_userStorage' - The size of the user volume.
--
-- 'description', 'workspaceBundle_description' - The description of the bundle.
newWorkspaceBundle ::
  WorkspaceBundle
newWorkspaceBundle =
  WorkspaceBundle'
    { creationTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      owner = Prelude.Nothing,
      rootStorage = Prelude.Nothing,
      name = Prelude.Nothing,
      imageId = Prelude.Nothing,
      computeType = Prelude.Nothing,
      userStorage = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The time when the bundle was created.
workspaceBundle_creationTime :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.UTCTime)
workspaceBundle_creationTime = Lens.lens (\WorkspaceBundle' {creationTime} -> creationTime) (\s@WorkspaceBundle' {} a -> s {creationTime = a} :: WorkspaceBundle) Prelude.. Lens.mapping Core._Time

-- | The last time that the bundle was updated.
workspaceBundle_lastUpdatedTime :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.UTCTime)
workspaceBundle_lastUpdatedTime = Lens.lens (\WorkspaceBundle' {lastUpdatedTime} -> lastUpdatedTime) (\s@WorkspaceBundle' {} a -> s {lastUpdatedTime = a} :: WorkspaceBundle) Prelude.. Lens.mapping Core._Time

-- | The identifier of the bundle.
workspaceBundle_bundleId :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_bundleId = Lens.lens (\WorkspaceBundle' {bundleId} -> bundleId) (\s@WorkspaceBundle' {} a -> s {bundleId = a} :: WorkspaceBundle)

-- | The owner of the bundle. This is the account identifier of the owner, or
-- @AMAZON@ if the bundle is provided by Amazon Web Services.
workspaceBundle_owner :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_owner = Lens.lens (\WorkspaceBundle' {owner} -> owner) (\s@WorkspaceBundle' {} a -> s {owner = a} :: WorkspaceBundle)

-- | The size of the root volume.
workspaceBundle_rootStorage :: Lens.Lens' WorkspaceBundle (Prelude.Maybe RootStorage)
workspaceBundle_rootStorage = Lens.lens (\WorkspaceBundle' {rootStorage} -> rootStorage) (\s@WorkspaceBundle' {} a -> s {rootStorage = a} :: WorkspaceBundle)

-- | The name of the bundle.
workspaceBundle_name :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_name = Lens.lens (\WorkspaceBundle' {name} -> name) (\s@WorkspaceBundle' {} a -> s {name = a} :: WorkspaceBundle)

-- | The identifier of the image that was used to create the bundle.
workspaceBundle_imageId :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_imageId = Lens.lens (\WorkspaceBundle' {imageId} -> imageId) (\s@WorkspaceBundle' {} a -> s {imageId = a} :: WorkspaceBundle)

-- | The compute type of the bundle. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceBundle_computeType :: Lens.Lens' WorkspaceBundle (Prelude.Maybe ComputeType)
workspaceBundle_computeType = Lens.lens (\WorkspaceBundle' {computeType} -> computeType) (\s@WorkspaceBundle' {} a -> s {computeType = a} :: WorkspaceBundle)

-- | The size of the user volume.
workspaceBundle_userStorage :: Lens.Lens' WorkspaceBundle (Prelude.Maybe UserStorage)
workspaceBundle_userStorage = Lens.lens (\WorkspaceBundle' {userStorage} -> userStorage) (\s@WorkspaceBundle' {} a -> s {userStorage = a} :: WorkspaceBundle)

-- | The description of the bundle.
workspaceBundle_description :: Lens.Lens' WorkspaceBundle (Prelude.Maybe Prelude.Text)
workspaceBundle_description = Lens.lens (\WorkspaceBundle' {description} -> description) (\s@WorkspaceBundle' {} a -> s {description = a} :: WorkspaceBundle)

instance Core.FromJSON WorkspaceBundle where
  parseJSON =
    Core.withObject
      "WorkspaceBundle"
      ( \x ->
          WorkspaceBundle'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "BundleId")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "RootStorage")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ImageId")
            Prelude.<*> (x Core..:? "ComputeType")
            Prelude.<*> (x Core..:? "UserStorage")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable WorkspaceBundle

instance Prelude.NFData WorkspaceBundle
