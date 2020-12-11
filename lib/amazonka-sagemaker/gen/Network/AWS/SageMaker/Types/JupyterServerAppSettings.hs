-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.JupyterServerAppSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.JupyterServerAppSettings
  ( JupyterServerAppSettings (..),

    -- * Smart constructor
    mkJupyterServerAppSettings,

    -- * Lenses
    jsasDefaultResourceSpec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The JupyterServer app settings.
--
-- /See:/ 'mkJupyterServerAppSettings' smart constructor.
newtype JupyterServerAppSettings = JupyterServerAppSettings'
  { defaultResourceSpec ::
      Lude.Maybe ResourceSpec
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JupyterServerAppSettings' with the minimum fields required to make a request.
--
-- * 'defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the JupyterServer app.
mkJupyterServerAppSettings ::
  JupyterServerAppSettings
mkJupyterServerAppSettings =
  JupyterServerAppSettings' {defaultResourceSpec = Lude.Nothing}

-- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the JupyterServer app.
--
-- /Note:/ Consider using 'defaultResourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsasDefaultResourceSpec :: Lens.Lens' JupyterServerAppSettings (Lude.Maybe ResourceSpec)
jsasDefaultResourceSpec = Lens.lens (defaultResourceSpec :: JupyterServerAppSettings -> Lude.Maybe ResourceSpec) (\s a -> s {defaultResourceSpec = a} :: JupyterServerAppSettings)
{-# DEPRECATED jsasDefaultResourceSpec "Use generic-lens or generic-optics with 'defaultResourceSpec' instead." #-}

instance Lude.FromJSON JupyterServerAppSettings where
  parseJSON =
    Lude.withObject
      "JupyterServerAppSettings"
      ( \x ->
          JupyterServerAppSettings'
            Lude.<$> (x Lude..:? "DefaultResourceSpec")
      )

instance Lude.ToJSON JupyterServerAppSettings where
  toJSON JupyterServerAppSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DefaultResourceSpec" Lude..=) Lude.<$> defaultResourceSpec]
      )
