-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Code
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Code
  ( Code (..),

    -- * Smart constructor
    mkCode,

    -- * Lenses
    cSource,
    cDestination,
  )
where

import Network.AWS.CodeStar.Types.CodeDestination
import Network.AWS.CodeStar.Types.CodeSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Location and destination information about the source code files provided with the project request. The source code is uploaded to the new project source repository after project creation.
--
-- /See:/ 'mkCode' smart constructor.
data Code = Code'
  { source :: CodeSource,
    destination :: CodeDestination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Code' with the minimum fields required to make a request.
--
-- * 'destination' - The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
-- * 'source' - The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
mkCode ::
  -- | 'source'
  CodeSource ->
  -- | 'destination'
  CodeDestination ->
  Code
mkCode pSource_ pDestination_ =
  Code' {source = pSource_, destination = pDestination_}

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSource :: Lens.Lens' Code CodeSource
cSource = Lens.lens (source :: Code -> CodeSource) (\s a -> s {source = a} :: Code)
{-# DEPRECATED cSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDestination :: Lens.Lens' Code CodeDestination
cDestination = Lens.lens (destination :: Code -> CodeDestination) (\s a -> s {destination = a} :: Code)
{-# DEPRECATED cDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.ToJSON Code where
  toJSON Code' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("source" Lude..= source),
            Lude.Just ("destination" Lude..= destination)
          ]
      )
