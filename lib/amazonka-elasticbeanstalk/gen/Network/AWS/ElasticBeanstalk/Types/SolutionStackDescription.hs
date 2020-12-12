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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the solution stack.
--
-- /See:/ 'mkSolutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
  { permittedFileTypes ::
      Lude.Maybe [Lude.Text],
    solutionStackName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SolutionStackDescription' with the minimum fields required to make a request.
--
-- * 'permittedFileTypes' - The permitted file types allowed for a solution stack.
-- * 'solutionStackName' - The name of the solution stack.
mkSolutionStackDescription ::
  SolutionStackDescription
mkSolutionStackDescription =
  SolutionStackDescription'
    { permittedFileTypes = Lude.Nothing,
      solutionStackName = Lude.Nothing
    }

-- | The permitted file types allowed for a solution stack.
--
-- /Note:/ Consider using 'permittedFileTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdPermittedFileTypes :: Lens.Lens' SolutionStackDescription (Lude.Maybe [Lude.Text])
ssdPermittedFileTypes = Lens.lens (permittedFileTypes :: SolutionStackDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {permittedFileTypes = a} :: SolutionStackDescription)
{-# DEPRECATED ssdPermittedFileTypes "Use generic-lens or generic-optics with 'permittedFileTypes' instead." #-}

-- | The name of the solution stack.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSolutionStackName :: Lens.Lens' SolutionStackDescription (Lude.Maybe Lude.Text)
ssdSolutionStackName = Lens.lens (solutionStackName :: SolutionStackDescription -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: SolutionStackDescription)
{-# DEPRECATED ssdSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

instance Lude.FromXML SolutionStackDescription where
  parseXML x =
    SolutionStackDescription'
      Lude.<$> ( x Lude..@? "PermittedFileTypes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "SolutionStackName")
