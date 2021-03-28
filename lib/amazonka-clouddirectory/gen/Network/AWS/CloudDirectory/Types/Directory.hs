{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Directory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.Directory
  ( Directory (..)
  -- * Smart constructor
  , mkDirectory
  -- * Lenses
  , dCreationDateTime
  , dDirectoryArn
  , dName
  , dState
  ) where

import qualified Network.AWS.CloudDirectory.Types.DirectoryArn as Types
import qualified Network.AWS.CloudDirectory.Types.DirectoryName as Types
import qualified Network.AWS.CloudDirectory.Types.DirectoryState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Directory structure that includes the directory name and directory ARN.
--
-- /See:/ 'mkDirectory' smart constructor.
data Directory = Directory'
  { creationDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the directory was created.
  , directoryArn :: Core.Maybe Types.DirectoryArn
    -- ^ The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
  , name :: Core.Maybe Types.DirectoryName
    -- ^ The name of the directory.
  , state :: Core.Maybe Types.DirectoryState
    -- ^ The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Directory' value with any optional fields omitted.
mkDirectory
    :: Directory
mkDirectory
  = Directory'{creationDateTime = Core.Nothing,
               directoryArn = Core.Nothing, name = Core.Nothing,
               state = Core.Nothing}

-- | The date and time when the directory was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationDateTime :: Lens.Lens' Directory (Core.Maybe Core.NominalDiffTime)
dCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE dCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDirectoryArn :: Lens.Lens' Directory (Core.Maybe Types.DirectoryArn)
dDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE dDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The name of the directory.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Directory (Core.Maybe Types.DirectoryName)
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dState :: Lens.Lens' Directory (Core.Maybe Types.DirectoryState)
dState = Lens.field @"state"
{-# INLINEABLE dState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON Directory where
        parseJSON
          = Core.withObject "Directory" Core.$
              \ x ->
                Directory' Core.<$>
                  (x Core..:? "CreationDateTime") Core.<*> x Core..:? "DirectoryArn"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "State"
