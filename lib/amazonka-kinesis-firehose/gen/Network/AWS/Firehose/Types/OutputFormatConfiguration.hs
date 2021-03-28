{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OutputFormatConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.OutputFormatConfiguration
  ( OutputFormatConfiguration (..)
  -- * Smart constructor
  , mkOutputFormatConfiguration
  -- * Lenses
  , ofcSerializer
  ) where

import qualified Network.AWS.Firehose.Types.Serializer as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data before it writes it to Amazon S3. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'mkOutputFormatConfiguration' smart constructor.
newtype OutputFormatConfiguration = OutputFormatConfiguration'
  { serializer :: Core.Maybe Types.Serializer
    -- ^ Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputFormatConfiguration' value with any optional fields omitted.
mkOutputFormatConfiguration
    :: OutputFormatConfiguration
mkOutputFormatConfiguration
  = OutputFormatConfiguration'{serializer = Core.Nothing}

-- | Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
--
-- /Note:/ Consider using 'serializer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofcSerializer :: Lens.Lens' OutputFormatConfiguration (Core.Maybe Types.Serializer)
ofcSerializer = Lens.field @"serializer"
{-# INLINEABLE ofcSerializer #-}
{-# DEPRECATED serializer "Use generic-lens or generic-optics with 'serializer' instead"  #-}

instance Core.FromJSON OutputFormatConfiguration where
        toJSON OutputFormatConfiguration{..}
          = Core.object
              (Core.catMaybes [("Serializer" Core..=) Core.<$> serializer])

instance Core.FromJSON OutputFormatConfiguration where
        parseJSON
          = Core.withObject "OutputFormatConfiguration" Core.$
              \ x ->
                OutputFormatConfiguration' Core.<$> (x Core..:? "Serializer")
