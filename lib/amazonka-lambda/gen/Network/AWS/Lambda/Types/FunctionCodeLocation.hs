{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionCodeLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionCodeLocation
  ( FunctionCodeLocation (..),

    -- * Smart constructor
    mkFunctionCodeLocation,

    -- * Lenses
    fclLocation,
    fclRepositoryType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a function's deployment package.
--
-- /See:/ 'mkFunctionCodeLocation' smart constructor.
data FunctionCodeLocation = FunctionCodeLocation'
  { location ::
      Lude.Maybe Lude.Text,
    repositoryType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionCodeLocation' with the minimum fields required to make a request.
--
-- * 'location' - A presigned URL that you can use to download the deployment package.
-- * 'repositoryType' - The service that's hosting the file.
mkFunctionCodeLocation ::
  FunctionCodeLocation
mkFunctionCodeLocation =
  FunctionCodeLocation'
    { location = Lude.Nothing,
      repositoryType = Lude.Nothing
    }

-- | A presigned URL that you can use to download the deployment package.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fclLocation :: Lens.Lens' FunctionCodeLocation (Lude.Maybe Lude.Text)
fclLocation = Lens.lens (location :: FunctionCodeLocation -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: FunctionCodeLocation)
{-# DEPRECATED fclLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The service that's hosting the file.
--
-- /Note:/ Consider using 'repositoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fclRepositoryType :: Lens.Lens' FunctionCodeLocation (Lude.Maybe Lude.Text)
fclRepositoryType = Lens.lens (repositoryType :: FunctionCodeLocation -> Lude.Maybe Lude.Text) (\s a -> s {repositoryType = a} :: FunctionCodeLocation)
{-# DEPRECATED fclRepositoryType "Use generic-lens or generic-optics with 'repositoryType' instead." #-}

instance Lude.FromJSON FunctionCodeLocation where
  parseJSON =
    Lude.withObject
      "FunctionCodeLocation"
      ( \x ->
          FunctionCodeLocation'
            Lude.<$> (x Lude..:? "Location") Lude.<*> (x Lude..:? "RepositoryType")
      )
