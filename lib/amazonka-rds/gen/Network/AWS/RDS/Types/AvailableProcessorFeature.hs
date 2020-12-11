-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AvailableProcessorFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AvailableProcessorFeature
  ( AvailableProcessorFeature (..),

    -- * Smart constructor
    mkAvailableProcessorFeature,

    -- * Lenses
    apfName,
    apfDefaultValue,
    apfAllowedValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the available processor feature information for the DB instance class of a DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class> in the /Amazon RDS User Guide. /
--
-- /See:/ 'mkAvailableProcessorFeature' smart constructor.
data AvailableProcessorFeature = AvailableProcessorFeature'
  { name ::
      Lude.Maybe Lude.Text,
    defaultValue :: Lude.Maybe Lude.Text,
    allowedValues :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailableProcessorFeature' with the minimum fields required to make a request.
--
-- * 'allowedValues' - The allowed values for the processor feature of the DB instance class.
-- * 'defaultValue' - The default value for the processor feature of the DB instance class.
-- * 'name' - The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
mkAvailableProcessorFeature ::
  AvailableProcessorFeature
mkAvailableProcessorFeature =
  AvailableProcessorFeature'
    { name = Lude.Nothing,
      defaultValue = Lude.Nothing,
      allowedValues = Lude.Nothing
    }

-- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apfName :: Lens.Lens' AvailableProcessorFeature (Lude.Maybe Lude.Text)
apfName = Lens.lens (name :: AvailableProcessorFeature -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AvailableProcessorFeature)
{-# DEPRECATED apfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The default value for the processor feature of the DB instance class.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apfDefaultValue :: Lens.Lens' AvailableProcessorFeature (Lude.Maybe Lude.Text)
apfDefaultValue = Lens.lens (defaultValue :: AvailableProcessorFeature -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: AvailableProcessorFeature)
{-# DEPRECATED apfDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The allowed values for the processor feature of the DB instance class.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apfAllowedValues :: Lens.Lens' AvailableProcessorFeature (Lude.Maybe Lude.Text)
apfAllowedValues = Lens.lens (allowedValues :: AvailableProcessorFeature -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: AvailableProcessorFeature)
{-# DEPRECATED apfAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

instance Lude.FromXML AvailableProcessorFeature where
  parseXML x =
    AvailableProcessorFeature'
      Lude.<$> (x Lude..@? "Name")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "AllowedValues")
