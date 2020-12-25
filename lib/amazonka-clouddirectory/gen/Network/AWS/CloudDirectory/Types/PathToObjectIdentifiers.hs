{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
  ( PathToObjectIdentifiers (..),

    -- * Smart constructor
    mkPathToObjectIdentifiers,

    -- * Lenses
    ptoiObjectIdentifiers,
    ptoiPath,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.CloudDirectory.Types.PathString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns the path to the @ObjectIdentifiers@ that is associated with the directory.
--
-- /See:/ 'mkPathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
  { -- | Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
    objectIdentifiers :: Core.Maybe [Types.ObjectIdentifier],
    -- | The path that is used to identify the object starting from directory root.
    path :: Core.Maybe Types.PathString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PathToObjectIdentifiers' value with any optional fields omitted.
mkPathToObjectIdentifiers ::
  PathToObjectIdentifiers
mkPathToObjectIdentifiers =
  PathToObjectIdentifiers'
    { objectIdentifiers = Core.Nothing,
      path = Core.Nothing
    }

-- | Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptoiObjectIdentifiers :: Lens.Lens' PathToObjectIdentifiers (Core.Maybe [Types.ObjectIdentifier])
ptoiObjectIdentifiers = Lens.field @"objectIdentifiers"
{-# DEPRECATED ptoiObjectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead." #-}

-- | The path that is used to identify the object starting from directory root.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptoiPath :: Lens.Lens' PathToObjectIdentifiers (Core.Maybe Types.PathString)
ptoiPath = Lens.field @"path"
{-# DEPRECATED ptoiPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.FromJSON PathToObjectIdentifiers where
  parseJSON =
    Core.withObject "PathToObjectIdentifiers" Core.$
      \x ->
        PathToObjectIdentifiers'
          Core.<$> (x Core..:? "ObjectIdentifiers") Core.<*> (x Core..:? "Path")
