-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
  ( ConfigurationOptionSetting (..),

    -- * Smart constructor
    mkConfigurationOptionSetting,

    -- * Lenses
    cosOptionName,
    cosResourceName,
    cosNamespace,
    cosValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A specification identifying an individual configuration option along with its current value. For a list of possible namespaces and option values, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /See:/ 'mkConfigurationOptionSetting' smart constructor.
data ConfigurationOptionSetting = ConfigurationOptionSetting'
  { optionName ::
      Lude.Maybe Lude.Text,
    resourceName :: Lude.Maybe Lude.Text,
    namespace :: Lude.Maybe Lude.Text,
    value :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationOptionSetting' with the minimum fields required to make a request.
--
-- * 'namespace' - A unique namespace that identifies the option's associated AWS resource.
-- * 'optionName' - The name of the configuration option.
-- * 'resourceName' - A unique resource name for the option setting. Use it for a time–based scaling configuration option.
-- * 'value' - The current value for the configuration option.
mkConfigurationOptionSetting ::
  ConfigurationOptionSetting
mkConfigurationOptionSetting =
  ConfigurationOptionSetting'
    { optionName = Lude.Nothing,
      resourceName = Lude.Nothing,
      namespace = Lude.Nothing,
      value = Lude.Nothing
    }

-- | The name of the configuration option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosOptionName :: Lens.Lens' ConfigurationOptionSetting (Lude.Maybe Lude.Text)
cosOptionName = Lens.lens (optionName :: ConfigurationOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {optionName = a} :: ConfigurationOptionSetting)
{-# DEPRECATED cosOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | A unique resource name for the option setting. Use it for a time–based scaling configuration option.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosResourceName :: Lens.Lens' ConfigurationOptionSetting (Lude.Maybe Lude.Text)
cosResourceName = Lens.lens (resourceName :: ConfigurationOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: ConfigurationOptionSetting)
{-# DEPRECATED cosResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | A unique namespace that identifies the option's associated AWS resource.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosNamespace :: Lens.Lens' ConfigurationOptionSetting (Lude.Maybe Lude.Text)
cosNamespace = Lens.lens (namespace :: ConfigurationOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: ConfigurationOptionSetting)
{-# DEPRECATED cosNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The current value for the configuration option.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosValue :: Lens.Lens' ConfigurationOptionSetting (Lude.Maybe Lude.Text)
cosValue = Lens.lens (value :: ConfigurationOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ConfigurationOptionSetting)
{-# DEPRECATED cosValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML ConfigurationOptionSetting where
  parseXML x =
    ConfigurationOptionSetting'
      Lude.<$> (x Lude..@? "OptionName")
      Lude.<*> (x Lude..@? "ResourceName")
      Lude.<*> (x Lude..@? "Namespace")
      Lude.<*> (x Lude..@? "Value")

instance Lude.ToQuery ConfigurationOptionSetting where
  toQuery ConfigurationOptionSetting' {..} =
    Lude.mconcat
      [ "OptionName" Lude.=: optionName,
        "ResourceName" Lude.=: resourceName,
        "Namespace" Lude.=: namespace,
        "Value" Lude.=: value
      ]
