-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DevEndpointCustomLibraries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DevEndpointCustomLibraries
  ( DevEndpointCustomLibraries (..),

    -- * Smart constructor
    mkDevEndpointCustomLibraries,

    -- * Lenses
    declExtraPythonLibsS3Path,
    declExtraJARsS3Path,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Custom libraries to be loaded into a development endpoint.
--
-- /See:/ 'mkDevEndpointCustomLibraries' smart constructor.
data DevEndpointCustomLibraries = DevEndpointCustomLibraries'
  { extraPythonLibsS3Path ::
      Lude.Maybe Lude.Text,
    extraJARsS3Path ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DevEndpointCustomLibraries' with the minimum fields required to make a request.
--
-- * 'extraJARsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
-- * 'extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon Simple Storage Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
mkDevEndpointCustomLibraries ::
  DevEndpointCustomLibraries
mkDevEndpointCustomLibraries =
  DevEndpointCustomLibraries'
    { extraPythonLibsS3Path = Lude.Nothing,
      extraJARsS3Path = Lude.Nothing
    }

-- | The paths to one or more Python libraries in an Amazon Simple Storage Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
declExtraPythonLibsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Lude.Maybe Lude.Text)
declExtraPythonLibsS3Path = Lens.lens (extraPythonLibsS3Path :: DevEndpointCustomLibraries -> Lude.Maybe Lude.Text) (\s a -> s {extraPythonLibsS3Path = a} :: DevEndpointCustomLibraries)
{-# DEPRECATED declExtraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead." #-}

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJARsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
declExtraJARsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Lude.Maybe Lude.Text)
declExtraJARsS3Path = Lens.lens (extraJARsS3Path :: DevEndpointCustomLibraries -> Lude.Maybe Lude.Text) (\s a -> s {extraJARsS3Path = a} :: DevEndpointCustomLibraries)
{-# DEPRECATED declExtraJARsS3Path "Use generic-lens or generic-optics with 'extraJARsS3Path' instead." #-}

instance Lude.ToJSON DevEndpointCustomLibraries where
  toJSON DevEndpointCustomLibraries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExtraPythonLibsS3Path" Lude..=) Lude.<$> extraPythonLibsS3Path,
            ("ExtraJarsS3Path" Lude..=) Lude.<$> extraJARsS3Path
          ]
      )
