-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemPolicyDescription
  ( FileSystemPolicyDescription (..),

    -- * Smart constructor
    mkFileSystemPolicyDescription,

    -- * Lenses
    fspdFileSystemId,
    fspdPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkFileSystemPolicyDescription' smart constructor.
data FileSystemPolicyDescription = FileSystemPolicyDescription'
  { fileSystemId ::
      Lude.Maybe Lude.Text,
    policy :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSystemPolicyDescription' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - Specifies the EFS file system to which the @FileSystemPolicy@ applies.
-- * 'policy' - The JSON formatted @FileSystemPolicy@ for the EFS file system.
mkFileSystemPolicyDescription ::
  FileSystemPolicyDescription
mkFileSystemPolicyDescription =
  FileSystemPolicyDescription'
    { fileSystemId = Lude.Nothing,
      policy = Lude.Nothing
    }

-- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fspdFileSystemId :: Lens.Lens' FileSystemPolicyDescription (Lude.Maybe Lude.Text)
fspdFileSystemId = Lens.lens (fileSystemId :: FileSystemPolicyDescription -> Lude.Maybe Lude.Text) (\s a -> s {fileSystemId = a} :: FileSystemPolicyDescription)
{-# DEPRECATED fspdFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fspdPolicy :: Lens.Lens' FileSystemPolicyDescription (Lude.Maybe Lude.Text)
fspdPolicy = Lens.lens (policy :: FileSystemPolicyDescription -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: FileSystemPolicyDescription)
{-# DEPRECATED fspdPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.FromJSON FileSystemPolicyDescription where
  parseJSON =
    Lude.withObject
      "FileSystemPolicyDescription"
      ( \x ->
          FileSystemPolicyDescription'
            Lude.<$> (x Lude..:? "FileSystemId") Lude.<*> (x Lude..:? "Policy")
      )
