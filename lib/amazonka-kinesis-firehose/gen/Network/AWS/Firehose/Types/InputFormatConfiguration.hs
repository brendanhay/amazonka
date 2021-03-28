{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.InputFormatConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.InputFormatConfiguration
  ( InputFormatConfiguration (..)
  -- * Smart constructor
  , mkInputFormatConfiguration
  -- * Lenses
  , ifcDeserializer
  ) where

import qualified Network.AWS.Firehose.Types.Deserializer as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the deserializer you want to use to convert the format of the input data. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'mkInputFormatConfiguration' smart constructor.
newtype InputFormatConfiguration = InputFormatConfiguration'
  { deserializer :: Core.Maybe Types.Deserializer
    -- ^ Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputFormatConfiguration' value with any optional fields omitted.
mkInputFormatConfiguration
    :: InputFormatConfiguration
mkInputFormatConfiguration
  = InputFormatConfiguration'{deserializer = Core.Nothing}

-- | Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
--
-- /Note:/ Consider using 'deserializer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcDeserializer :: Lens.Lens' InputFormatConfiguration (Core.Maybe Types.Deserializer)
ifcDeserializer = Lens.field @"deserializer"
{-# INLINEABLE ifcDeserializer #-}
{-# DEPRECATED deserializer "Use generic-lens or generic-optics with 'deserializer' instead"  #-}

instance Core.FromJSON InputFormatConfiguration where
        toJSON InputFormatConfiguration{..}
          = Core.object
              (Core.catMaybes [("Deserializer" Core..=) Core.<$> deserializer])

instance Core.FromJSON InputFormatConfiguration where
        parseJSON
          = Core.withObject "InputFormatConfiguration" Core.$
              \ x ->
                InputFormatConfiguration' Core.<$> (x Core..:? "Deserializer")
