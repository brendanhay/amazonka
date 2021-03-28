{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DataFormatConversionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DataFormatConversionConfiguration
  ( DataFormatConversionConfiguration (..)
  -- * Smart constructor
  , mkDataFormatConversionConfiguration
  -- * Lenses
  , dfccEnabled
  , dfccInputFormatConfiguration
  , dfccOutputFormatConfiguration
  , dfccSchemaConfiguration
  ) where

import qualified Network.AWS.Firehose.Types.InputFormatConfiguration as Types
import qualified Network.AWS.Firehose.Types.OutputFormatConfiguration as Types
import qualified Network.AWS.Firehose.Types.SchemaConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies that you want Kinesis Data Firehose to convert data from the JSON format to the Parquet or ORC format before writing it to Amazon S3. Kinesis Data Firehose uses the serializer and deserializer that you specify, in addition to the column information from the AWS Glue table, to deserialize your input data from JSON and then serialize it to the Parquet or ORC format. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/record-format-conversion.html Kinesis Data Firehose Record Format Conversion> .
--
-- /See:/ 'mkDataFormatConversionConfiguration' smart constructor.
data DataFormatConversionConfiguration = DataFormatConversionConfiguration'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
  , inputFormatConfiguration :: Core.Maybe Types.InputFormatConfiguration
    -- ^ Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON. This parameter is required if @Enabled@ is set to true.
  , outputFormatConfiguration :: Core.Maybe Types.OutputFormatConfiguration
    -- ^ Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format. This parameter is required if @Enabled@ is set to true.
  , schemaConfiguration :: Core.Maybe Types.SchemaConfiguration
    -- ^ Specifies the AWS Glue Data Catalog table that contains the column information. This parameter is required if @Enabled@ is set to true.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataFormatConversionConfiguration' value with any optional fields omitted.
mkDataFormatConversionConfiguration
    :: DataFormatConversionConfiguration
mkDataFormatConversionConfiguration
  = DataFormatConversionConfiguration'{enabled = Core.Nothing,
                                       inputFormatConfiguration = Core.Nothing,
                                       outputFormatConfiguration = Core.Nothing,
                                       schemaConfiguration = Core.Nothing}

-- | Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccEnabled :: Lens.Lens' DataFormatConversionConfiguration (Core.Maybe Core.Bool)
dfccEnabled = Lens.field @"enabled"
{-# INLINEABLE dfccEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON. This parameter is required if @Enabled@ is set to true.
--
-- /Note:/ Consider using 'inputFormatConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccInputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Core.Maybe Types.InputFormatConfiguration)
dfccInputFormatConfiguration = Lens.field @"inputFormatConfiguration"
{-# INLINEABLE dfccInputFormatConfiguration #-}
{-# DEPRECATED inputFormatConfiguration "Use generic-lens or generic-optics with 'inputFormatConfiguration' instead"  #-}

-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format. This parameter is required if @Enabled@ is set to true.
--
-- /Note:/ Consider using 'outputFormatConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccOutputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Core.Maybe Types.OutputFormatConfiguration)
dfccOutputFormatConfiguration = Lens.field @"outputFormatConfiguration"
{-# INLINEABLE dfccOutputFormatConfiguration #-}
{-# DEPRECATED outputFormatConfiguration "Use generic-lens or generic-optics with 'outputFormatConfiguration' instead"  #-}

-- | Specifies the AWS Glue Data Catalog table that contains the column information. This parameter is required if @Enabled@ is set to true.
--
-- /Note:/ Consider using 'schemaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccSchemaConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Core.Maybe Types.SchemaConfiguration)
dfccSchemaConfiguration = Lens.field @"schemaConfiguration"
{-# INLINEABLE dfccSchemaConfiguration #-}
{-# DEPRECATED schemaConfiguration "Use generic-lens or generic-optics with 'schemaConfiguration' instead"  #-}

instance Core.FromJSON DataFormatConversionConfiguration where
        toJSON DataFormatConversionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("Enabled" Core..=) Core.<$> enabled,
                  ("InputFormatConfiguration" Core..=) Core.<$>
                    inputFormatConfiguration,
                  ("OutputFormatConfiguration" Core..=) Core.<$>
                    outputFormatConfiguration,
                  ("SchemaConfiguration" Core..=) Core.<$> schemaConfiguration])

instance Core.FromJSON DataFormatConversionConfiguration where
        parseJSON
          = Core.withObject "DataFormatConversionConfiguration" Core.$
              \ x ->
                DataFormatConversionConfiguration' Core.<$>
                  (x Core..:? "Enabled") Core.<*>
                    x Core..:? "InputFormatConfiguration"
                    Core.<*> x Core..:? "OutputFormatConfiguration"
                    Core.<*> x Core..:? "SchemaConfiguration"
