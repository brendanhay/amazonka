{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.RootDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.RootDirectory
  ( RootDirectory (..),

    -- * Smart constructor
    mkRootDirectory,

    -- * Lenses
    rdCreationInfo,
    rdPath,
  )
where

import Network.AWS.EFS.Types.CreationInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the directory on the Amazon EFS file system that the access point provides access to. The access point exposes the specified file system path as the root directory of your file system to applications using the access point. NFS clients using the access point can only access data in the access point's @RootDirectory@ and it's subdirectories.
--
-- /See:/ 'mkRootDirectory' smart constructor.
data RootDirectory = RootDirectory'
  { -- | (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties.
    --
    -- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
    creationInfo :: Lude.Maybe CreationInfo,
    -- | Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
    path :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RootDirectory' with the minimum fields required to make a request.
--
-- * 'creationInfo' - (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties.
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
-- * 'path' - Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
mkRootDirectory ::
  RootDirectory
mkRootDirectory =
  RootDirectory' {creationInfo = Lude.Nothing, path = Lude.Nothing}

-- | (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties.
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
--
-- /Note:/ Consider using 'creationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreationInfo :: Lens.Lens' RootDirectory (Lude.Maybe CreationInfo)
rdCreationInfo = Lens.lens (creationInfo :: RootDirectory -> Lude.Maybe CreationInfo) (\s a -> s {creationInfo = a} :: RootDirectory)
{-# DEPRECATED rdCreationInfo "Use generic-lens or generic-optics with 'creationInfo' instead." #-}

-- | Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPath :: Lens.Lens' RootDirectory (Lude.Maybe Lude.Text)
rdPath = Lens.lens (path :: RootDirectory -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: RootDirectory)
{-# DEPRECATED rdPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Lude.FromJSON RootDirectory where
  parseJSON =
    Lude.withObject
      "RootDirectory"
      ( \x ->
          RootDirectory'
            Lude.<$> (x Lude..:? "CreationInfo") Lude.<*> (x Lude..:? "Path")
      )

instance Lude.ToJSON RootDirectory where
  toJSON RootDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreationInfo" Lude..=) Lude.<$> creationInfo,
            ("Path" Lude..=) Lude.<$> path
          ]
      )
