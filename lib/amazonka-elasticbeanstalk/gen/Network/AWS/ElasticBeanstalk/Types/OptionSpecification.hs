-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionSpecification
  ( OptionSpecification (..),

    -- * Smart constructor
    mkOptionSpecification,

    -- * Lenses
    osOptionName,
    osResourceName,
    osNamespace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A specification identifying an individual configuration option.
--
-- /See:/ 'mkOptionSpecification' smart constructor.
data OptionSpecification = OptionSpecification'
  { optionName ::
      Lude.Maybe Lude.Text,
    resourceName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'OptionSpecification' with the minimum fields required to make a request.
--
-- * 'namespace' - A unique namespace identifying the option's associated AWS resource.
-- * 'optionName' - The name of the configuration option.
-- * 'resourceName' - A unique resource name for a time-based scaling configuration option.
mkOptionSpecification ::
  OptionSpecification
mkOptionSpecification =
  OptionSpecification'
    { optionName = Lude.Nothing,
      resourceName = Lude.Nothing,
      namespace = Lude.Nothing
    }

-- | The name of the configuration option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOptionName :: Lens.Lens' OptionSpecification (Lude.Maybe Lude.Text)
osOptionName = Lens.lens (optionName :: OptionSpecification -> Lude.Maybe Lude.Text) (\s a -> s {optionName = a} :: OptionSpecification)
{-# DEPRECATED osOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | A unique resource name for a time-based scaling configuration option.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osResourceName :: Lens.Lens' OptionSpecification (Lude.Maybe Lude.Text)
osResourceName = Lens.lens (resourceName :: OptionSpecification -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: OptionSpecification)
{-# DEPRECATED osResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | A unique namespace identifying the option's associated AWS resource.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osNamespace :: Lens.Lens' OptionSpecification (Lude.Maybe Lude.Text)
osNamespace = Lens.lens (namespace :: OptionSpecification -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: OptionSpecification)
{-# DEPRECATED osNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

instance Lude.ToQuery OptionSpecification where
  toQuery OptionSpecification' {..} =
    Lude.mconcat
      [ "OptionName" Lude.=: optionName,
        "ResourceName" Lude.=: resourceName,
        "Namespace" Lude.=: namespace
      ]
