{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DataFormatConversionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DataFormatConversionConfiguration
  ( DataFormatConversionConfiguration (..),

    -- * Smart constructor
    mkDataFormatConversionConfiguration,

    -- * Lenses
    dfccOutputFormatConfiguration,
    dfccEnabled,
    dfccSchemaConfiguration,
    dfccInputFormatConfiguration,
  )
where

import Network.AWS.Firehose.Types.InputFormatConfiguration
import Network.AWS.Firehose.Types.OutputFormatConfiguration
import Network.AWS.Firehose.Types.SchemaConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies that you want Kinesis Data Firehose to convert data from the JSON format to the Parquet or ORC format before writing it to Amazon S3. Kinesis Data Firehose uses the serializer and deserializer that you specify, in addition to the column information from the AWS Glue table, to deserialize your input data from JSON and then serialize it to the Parquet or ORC format. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/record-format-conversion.html Kinesis Data Firehose Record Format Conversion> .
--
-- /See:/ 'mkDataFormatConversionConfiguration' smart constructor.
data DataFormatConversionConfiguration = DataFormatConversionConfiguration'
  { -- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format. This parameter is required if @Enabled@ is set to true.
    outputFormatConfiguration :: Lude.Maybe OutputFormatConfiguration,
    -- | Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Specifies the AWS Glue Data Catalog table that contains the column information. This parameter is required if @Enabled@ is set to true.
    schemaConfiguration :: Lude.Maybe SchemaConfiguration,
    -- | Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON. This parameter is required if @Enabled@ is set to true.
    inputFormatConfiguration :: Lude.Maybe InputFormatConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataFormatConversionConfiguration' with the minimum fields required to make a request.
--
-- * 'outputFormatConfiguration' - Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format. This parameter is required if @Enabled@ is set to true.
-- * 'enabled' - Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
-- * 'schemaConfiguration' - Specifies the AWS Glue Data Catalog table that contains the column information. This parameter is required if @Enabled@ is set to true.
-- * 'inputFormatConfiguration' - Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON. This parameter is required if @Enabled@ is set to true.
mkDataFormatConversionConfiguration ::
  DataFormatConversionConfiguration
mkDataFormatConversionConfiguration =
  DataFormatConversionConfiguration'
    { outputFormatConfiguration =
        Lude.Nothing,
      enabled = Lude.Nothing,
      schemaConfiguration = Lude.Nothing,
      inputFormatConfiguration = Lude.Nothing
    }

-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format. This parameter is required if @Enabled@ is set to true.
--
-- /Note:/ Consider using 'outputFormatConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccOutputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Lude.Maybe OutputFormatConfiguration)
dfccOutputFormatConfiguration = Lens.lens (outputFormatConfiguration :: DataFormatConversionConfiguration -> Lude.Maybe OutputFormatConfiguration) (\s a -> s {outputFormatConfiguration = a} :: DataFormatConversionConfiguration)
{-# DEPRECATED dfccOutputFormatConfiguration "Use generic-lens or generic-optics with 'outputFormatConfiguration' instead." #-}

-- | Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccEnabled :: Lens.Lens' DataFormatConversionConfiguration (Lude.Maybe Lude.Bool)
dfccEnabled = Lens.lens (enabled :: DataFormatConversionConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: DataFormatConversionConfiguration)
{-# DEPRECATED dfccEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies the AWS Glue Data Catalog table that contains the column information. This parameter is required if @Enabled@ is set to true.
--
-- /Note:/ Consider using 'schemaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccSchemaConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Lude.Maybe SchemaConfiguration)
dfccSchemaConfiguration = Lens.lens (schemaConfiguration :: DataFormatConversionConfiguration -> Lude.Maybe SchemaConfiguration) (\s a -> s {schemaConfiguration = a} :: DataFormatConversionConfiguration)
{-# DEPRECATED dfccSchemaConfiguration "Use generic-lens or generic-optics with 'schemaConfiguration' instead." #-}

-- | Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON. This parameter is required if @Enabled@ is set to true.
--
-- /Note:/ Consider using 'inputFormatConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfccInputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Lude.Maybe InputFormatConfiguration)
dfccInputFormatConfiguration = Lens.lens (inputFormatConfiguration :: DataFormatConversionConfiguration -> Lude.Maybe InputFormatConfiguration) (\s a -> s {inputFormatConfiguration = a} :: DataFormatConversionConfiguration)
{-# DEPRECATED dfccInputFormatConfiguration "Use generic-lens or generic-optics with 'inputFormatConfiguration' instead." #-}

instance Lude.FromJSON DataFormatConversionConfiguration where
  parseJSON =
    Lude.withObject
      "DataFormatConversionConfiguration"
      ( \x ->
          DataFormatConversionConfiguration'
            Lude.<$> (x Lude..:? "OutputFormatConfiguration")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "SchemaConfiguration")
            Lude.<*> (x Lude..:? "InputFormatConfiguration")
      )

instance Lude.ToJSON DataFormatConversionConfiguration where
  toJSON DataFormatConversionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OutputFormatConfiguration" Lude..=)
              Lude.<$> outputFormatConfiguration,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("SchemaConfiguration" Lude..=) Lude.<$> schemaConfiguration,
            ("InputFormatConfiguration" Lude..=)
              Lude.<$> inputFormatConfiguration
          ]
      )
