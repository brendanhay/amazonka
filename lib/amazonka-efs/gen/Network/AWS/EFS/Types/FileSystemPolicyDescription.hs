{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.FileSystemPolicyDescription
  ( FileSystemPolicyDescription (..)
  -- * Smart constructor
  , mkFileSystemPolicyDescription
  -- * Lenses
  , fspdFileSystemId
  , fspdPolicy
  ) where

import qualified Network.AWS.EFS.Types.FileSystemId as Types
import qualified Network.AWS.EFS.Types.Policy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkFileSystemPolicyDescription' smart constructor.
data FileSystemPolicyDescription = FileSystemPolicyDescription'
  { fileSystemId :: Core.Maybe Types.FileSystemId
    -- ^ Specifies the EFS file system to which the @FileSystemPolicy@ applies.
  , policy :: Core.Maybe Types.Policy
    -- ^ The JSON formatted @FileSystemPolicy@ for the EFS file system.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileSystemPolicyDescription' value with any optional fields omitted.
mkFileSystemPolicyDescription
    :: FileSystemPolicyDescription
mkFileSystemPolicyDescription
  = FileSystemPolicyDescription'{fileSystemId = Core.Nothing,
                                 policy = Core.Nothing}

-- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fspdFileSystemId :: Lens.Lens' FileSystemPolicyDescription (Core.Maybe Types.FileSystemId)
fspdFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE fspdFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fspdPolicy :: Lens.Lens' FileSystemPolicyDescription (Core.Maybe Types.Policy)
fspdPolicy = Lens.field @"policy"
{-# INLINEABLE fspdPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

instance Core.FromJSON FileSystemPolicyDescription where
        parseJSON
          = Core.withObject "FileSystemPolicyDescription" Core.$
              \ x ->
                FileSystemPolicyDescription' Core.<$>
                  (x Core..:? "FileSystemId") Core.<*> x Core..:? "Policy"
