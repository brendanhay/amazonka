{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
  ( SolutionStackDescription (..),

    -- * Smart constructor
    mkSolutionStackDescription,

    -- * Lenses
    ssdPermittedFileTypes,
    ssdSolutionStackName,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.FileTypeExtension as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SolutionStackName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the solution stack.
--
-- /See:/ 'mkSolutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
  { -- | The permitted file types allowed for a solution stack.
    permittedFileTypes :: Core.Maybe [Types.FileTypeExtension],
    -- | The name of the solution stack.
    solutionStackName :: Core.Maybe Types.SolutionStackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SolutionStackDescription' value with any optional fields omitted.
mkSolutionStackDescription ::
  SolutionStackDescription
mkSolutionStackDescription =
  SolutionStackDescription'
    { permittedFileTypes = Core.Nothing,
      solutionStackName = Core.Nothing
    }

-- | The permitted file types allowed for a solution stack.
--
-- /Note:/ Consider using 'permittedFileTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdPermittedFileTypes :: Lens.Lens' SolutionStackDescription (Core.Maybe [Types.FileTypeExtension])
ssdPermittedFileTypes = Lens.field @"permittedFileTypes"
{-# DEPRECATED ssdPermittedFileTypes "Use generic-lens or generic-optics with 'permittedFileTypes' instead." #-}

-- | The name of the solution stack.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSolutionStackName :: Lens.Lens' SolutionStackDescription (Core.Maybe Types.SolutionStackName)
ssdSolutionStackName = Lens.field @"solutionStackName"
{-# DEPRECATED ssdSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

instance Core.FromXML SolutionStackDescription where
  parseXML x =
    SolutionStackDescription'
      Core.<$> ( x Core..@? "PermittedFileTypes"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "SolutionStackName")
