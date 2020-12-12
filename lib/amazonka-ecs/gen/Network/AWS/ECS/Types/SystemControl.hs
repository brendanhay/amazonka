{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SystemControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.SystemControl
  ( SystemControl (..),

    -- * Smart constructor
    mkSystemControl,

    -- * Lenses
    scValue,
    scNamespace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- It is not recommended that you specify network-related @systemControls@ parameters for multiple containers in a single task that also uses either the @awsvpc@ or @host@ network mode for the following reasons:
--
--     * For tasks that use the @awsvpc@ network mode, if you set @systemControls@ for any container, it applies to all containers in the task. If you set different @systemControls@ for multiple containers in a single task, the container that is started last determines which @systemControls@ take effect.
--
--
--     * For tasks that use the @host@ network mode, the @systemControls@ parameter applies to the container instance's kernel parameter as well as that of all containers of any tasks running on that container instance.
--
--
--
-- /See:/ 'mkSystemControl' smart constructor.
data SystemControl = SystemControl'
  { value :: Lude.Maybe Lude.Text,
    namespace :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SystemControl' with the minimum fields required to make a request.
--
-- * 'namespace' - The namespaced kernel parameter for which to set a @value@ .
-- * 'value' - The value for the namespaced kernel parameter specified in @namespace@ .
mkSystemControl ::
  SystemControl
mkSystemControl =
  SystemControl' {value = Lude.Nothing, namespace = Lude.Nothing}

-- | The value for the namespaced kernel parameter specified in @namespace@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scValue :: Lens.Lens' SystemControl (Lude.Maybe Lude.Text)
scValue = Lens.lens (value :: SystemControl -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: SystemControl)
{-# DEPRECATED scValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The namespaced kernel parameter for which to set a @value@ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNamespace :: Lens.Lens' SystemControl (Lude.Maybe Lude.Text)
scNamespace = Lens.lens (namespace :: SystemControl -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: SystemControl)
{-# DEPRECATED scNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

instance Lude.FromJSON SystemControl where
  parseJSON =
    Lude.withObject
      "SystemControl"
      ( \x ->
          SystemControl'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "namespace")
      )

instance Lude.ToJSON SystemControl where
  toJSON SystemControl' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("value" Lude..=) Lude.<$> value,
            ("namespace" Lude..=) Lude.<$> namespace
          ]
      )
