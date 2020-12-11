-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceBundle
  ( WorkspaceBundle (..),

    -- * Smart constructor
    mkWorkspaceBundle,

    -- * Lenses
    wbLastUpdatedTime,
    wbBundleId,
    wbOwner,
    wbRootStorage,
    wbName,
    wbImageId,
    wbComputeType,
    wbUserStorage,
    wbDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.UserStorage

-- | Describes a WorkSpace bundle.
--
-- /See:/ 'mkWorkspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { lastUpdatedTime ::
      Lude.Maybe Lude.Timestamp,
    bundleId :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    rootStorage :: Lude.Maybe RootStorage,
    name :: Lude.Maybe Lude.Text,
    imageId :: Lude.Maybe Lude.Text,
    computeType :: Lude.Maybe ComputeType,
    userStorage :: Lude.Maybe UserStorage,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceBundle' with the minimum fields required to make a request.
--
-- * 'bundleId' - The bundle identifier.
-- * 'computeType' - The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
-- * 'description' - A description.
-- * 'imageId' - The image identifier of the bundle.
-- * 'lastUpdatedTime' - The last time that the bundle was updated.
-- * 'name' - The name of the bundle.
-- * 'owner' - The owner of the bundle. This is the account identifier of the owner, or @AMAZON@ if the bundle is provided by AWS.
-- * 'rootStorage' - The size of the root volume.
-- * 'userStorage' - The size of the user storage.
mkWorkspaceBundle ::
  WorkspaceBundle
mkWorkspaceBundle =
  WorkspaceBundle'
    { lastUpdatedTime = Lude.Nothing,
      bundleId = Lude.Nothing,
      owner = Lude.Nothing,
      rootStorage = Lude.Nothing,
      name = Lude.Nothing,
      imageId = Lude.Nothing,
      computeType = Lude.Nothing,
      userStorage = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The last time that the bundle was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbLastUpdatedTime :: Lens.Lens' WorkspaceBundle (Lude.Maybe Lude.Timestamp)
wbLastUpdatedTime = Lens.lens (lastUpdatedTime :: WorkspaceBundle -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: WorkspaceBundle)
{-# DEPRECATED wbLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The bundle identifier.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbBundleId :: Lens.Lens' WorkspaceBundle (Lude.Maybe Lude.Text)
wbBundleId = Lens.lens (bundleId :: WorkspaceBundle -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: WorkspaceBundle)
{-# DEPRECATED wbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The owner of the bundle. This is the account identifier of the owner, or @AMAZON@ if the bundle is provided by AWS.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbOwner :: Lens.Lens' WorkspaceBundle (Lude.Maybe Lude.Text)
wbOwner = Lens.lens (owner :: WorkspaceBundle -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: WorkspaceBundle)
{-# DEPRECATED wbOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The size of the root volume.
--
-- /Note:/ Consider using 'rootStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbRootStorage :: Lens.Lens' WorkspaceBundle (Lude.Maybe RootStorage)
wbRootStorage = Lens.lens (rootStorage :: WorkspaceBundle -> Lude.Maybe RootStorage) (\s a -> s {rootStorage = a} :: WorkspaceBundle)
{-# DEPRECATED wbRootStorage "Use generic-lens or generic-optics with 'rootStorage' instead." #-}

-- | The name of the bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbName :: Lens.Lens' WorkspaceBundle (Lude.Maybe Lude.Text)
wbName = Lens.lens (name :: WorkspaceBundle -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WorkspaceBundle)
{-# DEPRECATED wbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The image identifier of the bundle.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbImageId :: Lens.Lens' WorkspaceBundle (Lude.Maybe Lude.Text)
wbImageId = Lens.lens (imageId :: WorkspaceBundle -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: WorkspaceBundle)
{-# DEPRECATED wbImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
--
-- /Note:/ Consider using 'computeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbComputeType :: Lens.Lens' WorkspaceBundle (Lude.Maybe ComputeType)
wbComputeType = Lens.lens (computeType :: WorkspaceBundle -> Lude.Maybe ComputeType) (\s a -> s {computeType = a} :: WorkspaceBundle)
{-# DEPRECATED wbComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | The size of the user storage.
--
-- /Note:/ Consider using 'userStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbUserStorage :: Lens.Lens' WorkspaceBundle (Lude.Maybe UserStorage)
wbUserStorage = Lens.lens (userStorage :: WorkspaceBundle -> Lude.Maybe UserStorage) (\s a -> s {userStorage = a} :: WorkspaceBundle)
{-# DEPRECATED wbUserStorage "Use generic-lens or generic-optics with 'userStorage' instead." #-}

-- | A description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbDescription :: Lens.Lens' WorkspaceBundle (Lude.Maybe Lude.Text)
wbDescription = Lens.lens (description :: WorkspaceBundle -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WorkspaceBundle)
{-# DEPRECATED wbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON WorkspaceBundle where
  parseJSON =
    Lude.withObject
      "WorkspaceBundle"
      ( \x ->
          WorkspaceBundle'
            Lude.<$> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "BundleId")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "RootStorage")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ImageId")
            Lude.<*> (x Lude..:? "ComputeType")
            Lude.<*> (x Lude..:? "UserStorage")
            Lude.<*> (x Lude..:? "Description")
      )
