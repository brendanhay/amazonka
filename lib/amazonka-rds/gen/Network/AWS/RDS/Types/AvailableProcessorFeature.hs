{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AvailableProcessorFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.AvailableProcessorFeature
  ( AvailableProcessorFeature (..)
  -- * Smart constructor
  , mkAvailableProcessorFeature
  -- * Lenses
  , apfAllowedValues
  , apfDefaultValue
  , apfName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the available processor feature information for the DB instance class of a DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class> in the /Amazon RDS User Guide. / 
--
-- /See:/ 'mkAvailableProcessorFeature' smart constructor.
data AvailableProcessorFeature = AvailableProcessorFeature'
  { allowedValues :: Core.Maybe Core.Text
    -- ^ The allowed values for the processor feature of the DB instance class.
  , defaultValue :: Core.Maybe Core.Text
    -- ^ The default value for the processor feature of the DB instance class.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailableProcessorFeature' value with any optional fields omitted.
mkAvailableProcessorFeature
    :: AvailableProcessorFeature
mkAvailableProcessorFeature
  = AvailableProcessorFeature'{allowedValues = Core.Nothing,
                               defaultValue = Core.Nothing, name = Core.Nothing}

-- | The allowed values for the processor feature of the DB instance class.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apfAllowedValues :: Lens.Lens' AvailableProcessorFeature (Core.Maybe Core.Text)
apfAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE apfAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

-- | The default value for the processor feature of the DB instance class.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apfDefaultValue :: Lens.Lens' AvailableProcessorFeature (Core.Maybe Core.Text)
apfDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE apfDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apfName :: Lens.Lens' AvailableProcessorFeature (Core.Maybe Core.Text)
apfName = Lens.field @"name"
{-# INLINEABLE apfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML AvailableProcessorFeature where
        parseXML x
          = AvailableProcessorFeature' Core.<$>
              (x Core..@? "AllowedValues") Core.<*> x Core..@? "DefaultValue"
                Core.<*> x Core..@? "Name"
