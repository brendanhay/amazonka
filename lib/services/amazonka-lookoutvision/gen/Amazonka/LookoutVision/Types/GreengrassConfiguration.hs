{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutVision.Types.GreengrassConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.GreengrassConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.S3Location
import Amazonka.LookoutVision.Types.Tag
import Amazonka.LookoutVision.Types.TargetDevice
import Amazonka.LookoutVision.Types.TargetPlatform
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for the AWS IoT Greengrass component created
-- in a model packaging job. For more information, see
-- StartModelPackagingJob.
--
-- You can\'t specify a component with the same @ComponentName@ and
-- @Componentversion@ as an existing component with the same component name
-- and component version.
--
-- /See:/ 'newGreengrassConfiguration' smart constructor.
data GreengrassConfiguration = GreengrassConfiguration'
  { -- | Additional compiler options for the Greengrass component. Currently,
    -- only NVIDIA Graphics Processing Units (GPU) and CPU accelerators are
    -- supported. If you specify @TargetDevice@, don\'t specify
    -- @CompilerOptions@.
    --
    -- For more information, see /Compiler options/ in the Amazon Lookout for
    -- Vision Developer Guide.
    compilerOptions :: Prelude.Maybe Prelude.Text,
    -- | A description for the AWS IoT Greengrass component.
    componentDescription :: Prelude.Maybe Prelude.Text,
    -- | A Version for the AWS IoT Greengrass component. If you don\'t provide a
    -- value, a default value of @ @/@Model Version@/@.0.0@ is used.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | A set of tags (key-value pairs) that you want to attach to the AWS IoT
    -- Greengrass component.
    tags :: Prelude.Maybe [Tag],
    -- | The target device for the model. Currently the only supported value is
    -- @jetson_xavier@. If you specify @TargetDevice@, you can\'t specify
    -- @TargetPlatform@.
    targetDevice :: Prelude.Maybe TargetDevice,
    -- | The target platform for the model. If you specify @TargetPlatform@, you
    -- can\'t specify @TargetDevice@.
    targetPlatform :: Prelude.Maybe TargetPlatform,
    -- | An S3 location in which Lookout for Vision stores the component
    -- artifacts.
    s3OutputLocation :: S3Location,
    -- | A name for the AWS IoT Greengrass component.
    componentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GreengrassConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilerOptions', 'greengrassConfiguration_compilerOptions' - Additional compiler options for the Greengrass component. Currently,
-- only NVIDIA Graphics Processing Units (GPU) and CPU accelerators are
-- supported. If you specify @TargetDevice@, don\'t specify
-- @CompilerOptions@.
--
-- For more information, see /Compiler options/ in the Amazon Lookout for
-- Vision Developer Guide.
--
-- 'componentDescription', 'greengrassConfiguration_componentDescription' - A description for the AWS IoT Greengrass component.
--
-- 'componentVersion', 'greengrassConfiguration_componentVersion' - A Version for the AWS IoT Greengrass component. If you don\'t provide a
-- value, a default value of @ @/@Model Version@/@.0.0@ is used.
--
-- 'tags', 'greengrassConfiguration_tags' - A set of tags (key-value pairs) that you want to attach to the AWS IoT
-- Greengrass component.
--
-- 'targetDevice', 'greengrassConfiguration_targetDevice' - The target device for the model. Currently the only supported value is
-- @jetson_xavier@. If you specify @TargetDevice@, you can\'t specify
-- @TargetPlatform@.
--
-- 'targetPlatform', 'greengrassConfiguration_targetPlatform' - The target platform for the model. If you specify @TargetPlatform@, you
-- can\'t specify @TargetDevice@.
--
-- 's3OutputLocation', 'greengrassConfiguration_s3OutputLocation' - An S3 location in which Lookout for Vision stores the component
-- artifacts.
--
-- 'componentName', 'greengrassConfiguration_componentName' - A name for the AWS IoT Greengrass component.
newGreengrassConfiguration ::
  -- | 's3OutputLocation'
  S3Location ->
  -- | 'componentName'
  Prelude.Text ->
  GreengrassConfiguration
newGreengrassConfiguration
  pS3OutputLocation_
  pComponentName_ =
    GreengrassConfiguration'
      { compilerOptions =
          Prelude.Nothing,
        componentDescription = Prelude.Nothing,
        componentVersion = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetDevice = Prelude.Nothing,
        targetPlatform = Prelude.Nothing,
        s3OutputLocation = pS3OutputLocation_,
        componentName = pComponentName_
      }

-- | Additional compiler options for the Greengrass component. Currently,
-- only NVIDIA Graphics Processing Units (GPU) and CPU accelerators are
-- supported. If you specify @TargetDevice@, don\'t specify
-- @CompilerOptions@.
--
-- For more information, see /Compiler options/ in the Amazon Lookout for
-- Vision Developer Guide.
greengrassConfiguration_compilerOptions :: Lens.Lens' GreengrassConfiguration (Prelude.Maybe Prelude.Text)
greengrassConfiguration_compilerOptions = Lens.lens (\GreengrassConfiguration' {compilerOptions} -> compilerOptions) (\s@GreengrassConfiguration' {} a -> s {compilerOptions = a} :: GreengrassConfiguration)

-- | A description for the AWS IoT Greengrass component.
greengrassConfiguration_componentDescription :: Lens.Lens' GreengrassConfiguration (Prelude.Maybe Prelude.Text)
greengrassConfiguration_componentDescription = Lens.lens (\GreengrassConfiguration' {componentDescription} -> componentDescription) (\s@GreengrassConfiguration' {} a -> s {componentDescription = a} :: GreengrassConfiguration)

-- | A Version for the AWS IoT Greengrass component. If you don\'t provide a
-- value, a default value of @ @/@Model Version@/@.0.0@ is used.
greengrassConfiguration_componentVersion :: Lens.Lens' GreengrassConfiguration (Prelude.Maybe Prelude.Text)
greengrassConfiguration_componentVersion = Lens.lens (\GreengrassConfiguration' {componentVersion} -> componentVersion) (\s@GreengrassConfiguration' {} a -> s {componentVersion = a} :: GreengrassConfiguration)

-- | A set of tags (key-value pairs) that you want to attach to the AWS IoT
-- Greengrass component.
greengrassConfiguration_tags :: Lens.Lens' GreengrassConfiguration (Prelude.Maybe [Tag])
greengrassConfiguration_tags = Lens.lens (\GreengrassConfiguration' {tags} -> tags) (\s@GreengrassConfiguration' {} a -> s {tags = a} :: GreengrassConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The target device for the model. Currently the only supported value is
-- @jetson_xavier@. If you specify @TargetDevice@, you can\'t specify
-- @TargetPlatform@.
greengrassConfiguration_targetDevice :: Lens.Lens' GreengrassConfiguration (Prelude.Maybe TargetDevice)
greengrassConfiguration_targetDevice = Lens.lens (\GreengrassConfiguration' {targetDevice} -> targetDevice) (\s@GreengrassConfiguration' {} a -> s {targetDevice = a} :: GreengrassConfiguration)

-- | The target platform for the model. If you specify @TargetPlatform@, you
-- can\'t specify @TargetDevice@.
greengrassConfiguration_targetPlatform :: Lens.Lens' GreengrassConfiguration (Prelude.Maybe TargetPlatform)
greengrassConfiguration_targetPlatform = Lens.lens (\GreengrassConfiguration' {targetPlatform} -> targetPlatform) (\s@GreengrassConfiguration' {} a -> s {targetPlatform = a} :: GreengrassConfiguration)

-- | An S3 location in which Lookout for Vision stores the component
-- artifacts.
greengrassConfiguration_s3OutputLocation :: Lens.Lens' GreengrassConfiguration S3Location
greengrassConfiguration_s3OutputLocation = Lens.lens (\GreengrassConfiguration' {s3OutputLocation} -> s3OutputLocation) (\s@GreengrassConfiguration' {} a -> s {s3OutputLocation = a} :: GreengrassConfiguration)

-- | A name for the AWS IoT Greengrass component.
greengrassConfiguration_componentName :: Lens.Lens' GreengrassConfiguration Prelude.Text
greengrassConfiguration_componentName = Lens.lens (\GreengrassConfiguration' {componentName} -> componentName) (\s@GreengrassConfiguration' {} a -> s {componentName = a} :: GreengrassConfiguration)

instance Data.FromJSON GreengrassConfiguration where
  parseJSON =
    Data.withObject
      "GreengrassConfiguration"
      ( \x ->
          GreengrassConfiguration'
            Prelude.<$> (x Data..:? "CompilerOptions")
            Prelude.<*> (x Data..:? "ComponentDescription")
            Prelude.<*> (x Data..:? "ComponentVersion")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TargetDevice")
            Prelude.<*> (x Data..:? "TargetPlatform")
            Prelude.<*> (x Data..: "S3OutputLocation")
            Prelude.<*> (x Data..: "ComponentName")
      )

instance Prelude.Hashable GreengrassConfiguration where
  hashWithSalt _salt GreengrassConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` compilerOptions
      `Prelude.hashWithSalt` componentDescription
      `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetDevice
      `Prelude.hashWithSalt` targetPlatform
      `Prelude.hashWithSalt` s3OutputLocation
      `Prelude.hashWithSalt` componentName

instance Prelude.NFData GreengrassConfiguration where
  rnf GreengrassConfiguration' {..} =
    Prelude.rnf compilerOptions
      `Prelude.seq` Prelude.rnf componentDescription
      `Prelude.seq` Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetDevice
      `Prelude.seq` Prelude.rnf targetPlatform
      `Prelude.seq` Prelude.rnf s3OutputLocation
      `Prelude.seq` Prelude.rnf componentName

instance Data.ToJSON GreengrassConfiguration where
  toJSON GreengrassConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompilerOptions" Data..=)
              Prelude.<$> compilerOptions,
            ("ComponentDescription" Data..=)
              Prelude.<$> componentDescription,
            ("ComponentVersion" Data..=)
              Prelude.<$> componentVersion,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetDevice" Data..=) Prelude.<$> targetDevice,
            ("TargetPlatform" Data..=)
              Prelude.<$> targetPlatform,
            Prelude.Just
              ("S3OutputLocation" Data..= s3OutputLocation),
            Prelude.Just
              ("ComponentName" Data..= componentName)
          ]
      )
