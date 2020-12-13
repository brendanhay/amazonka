{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Directory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Directory
  ( Directory (..),

    -- * Smart constructor
    mkDirectory,

    -- * Lenses
    dDirectoryARN,
    dState,
    dName,
    dCreationDateTime,
  )
where

import Network.AWS.CloudDirectory.Types.DirectoryState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Directory structure that includes the directory name and directory ARN.
--
-- /See:/ 'mkDirectory' smart constructor.
data Directory = Directory'
  { -- | The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
    directoryARN :: Lude.Maybe Lude.Text,
    -- | The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
    state :: Lude.Maybe DirectoryState,
    -- | The name of the directory.
    name :: Lude.Maybe Lude.Text,
    -- | The date and time when the directory was created.
    creationDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Directory' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
-- * 'state' - The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
-- * 'name' - The name of the directory.
-- * 'creationDateTime' - The date and time when the directory was created.
mkDirectory ::
  Directory
mkDirectory =
  Directory'
    { directoryARN = Lude.Nothing,
      state = Lude.Nothing,
      name = Lude.Nothing,
      creationDateTime = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDirectoryARN :: Lens.Lens' Directory (Lude.Maybe Lude.Text)
dDirectoryARN = Lens.lens (directoryARN :: Directory -> Lude.Maybe Lude.Text) (\s a -> s {directoryARN = a} :: Directory)
{-# DEPRECATED dDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dState :: Lens.Lens' Directory (Lude.Maybe DirectoryState)
dState = Lens.lens (state :: Directory -> Lude.Maybe DirectoryState) (\s a -> s {state = a} :: Directory)
{-# DEPRECATED dState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the directory.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Directory (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: Directory -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Directory)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time when the directory was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationDateTime :: Lens.Lens' Directory (Lude.Maybe Lude.Timestamp)
dCreationDateTime = Lens.lens (creationDateTime :: Directory -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: Directory)
{-# DEPRECATED dCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

instance Lude.FromJSON Directory where
  parseJSON =
    Lude.withObject
      "Directory"
      ( \x ->
          Directory'
            Lude.<$> (x Lude..:? "DirectoryArn")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "CreationDateTime")
      )
