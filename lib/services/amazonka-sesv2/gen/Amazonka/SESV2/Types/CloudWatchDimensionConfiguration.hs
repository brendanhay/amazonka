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
-- Module      : Amazonka.SESV2.Types.CloudWatchDimensionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.CloudWatchDimensionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.DimensionValueSource

-- | An object that defines the dimension configuration to use when you send
-- email events to Amazon CloudWatch.
--
-- /See:/ 'newCloudWatchDimensionConfiguration' smart constructor.
data CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration'
  { -- | The name of an Amazon CloudWatch dimension associated with an email
    -- sending metric. The name has to meet the following criteria:
    --
    -- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
    --     underscores (_), or dashes (-).
    --
    -- -   It can contain no more than 256 characters.
    dimensionName :: Prelude.Text,
    -- | The location where the Amazon SES API v2 finds the value of a dimension
    -- to publish to Amazon CloudWatch. To use the message tags that you
    -- specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the
    -- @SendEmail@ or @SendRawEmail@ API, choose @messageTag@. To use your own
    -- email headers, choose @emailHeader@. To use link tags, choose
    -- @linkTags@.
    dimensionValueSource :: DimensionValueSource,
    -- | The default value of the dimension that is published to Amazon
    -- CloudWatch if you don\'t provide the value of the dimension when you
    -- send an email. This value has to meet the following criteria:
    --
    -- -   Can only contain ASCII letters (a–z, A–Z), numbers (0–9),
    --     underscores (_), or dashes (-), at signs (\@), and periods (.).
    --
    -- -   It can contain no more than 256 characters.
    defaultDimensionValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchDimensionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionName', 'cloudWatchDimensionConfiguration_dimensionName' - The name of an Amazon CloudWatch dimension associated with an email
-- sending metric. The name has to meet the following criteria:
--
-- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-).
--
-- -   It can contain no more than 256 characters.
--
-- 'dimensionValueSource', 'cloudWatchDimensionConfiguration_dimensionValueSource' - The location where the Amazon SES API v2 finds the value of a dimension
-- to publish to Amazon CloudWatch. To use the message tags that you
-- specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the
-- @SendEmail@ or @SendRawEmail@ API, choose @messageTag@. To use your own
-- email headers, choose @emailHeader@. To use link tags, choose
-- @linkTags@.
--
-- 'defaultDimensionValue', 'cloudWatchDimensionConfiguration_defaultDimensionValue' - The default value of the dimension that is published to Amazon
-- CloudWatch if you don\'t provide the value of the dimension when you
-- send an email. This value has to meet the following criteria:
--
-- -   Can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-), at signs (\@), and periods (.).
--
-- -   It can contain no more than 256 characters.
newCloudWatchDimensionConfiguration ::
  -- | 'dimensionName'
  Prelude.Text ->
  -- | 'dimensionValueSource'
  DimensionValueSource ->
  -- | 'defaultDimensionValue'
  Prelude.Text ->
  CloudWatchDimensionConfiguration
newCloudWatchDimensionConfiguration
  pDimensionName_
  pDimensionValueSource_
  pDefaultDimensionValue_ =
    CloudWatchDimensionConfiguration'
      { dimensionName =
          pDimensionName_,
        dimensionValueSource =
          pDimensionValueSource_,
        defaultDimensionValue =
          pDefaultDimensionValue_
      }

-- | The name of an Amazon CloudWatch dimension associated with an email
-- sending metric. The name has to meet the following criteria:
--
-- -   It can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-).
--
-- -   It can contain no more than 256 characters.
cloudWatchDimensionConfiguration_dimensionName :: Lens.Lens' CloudWatchDimensionConfiguration Prelude.Text
cloudWatchDimensionConfiguration_dimensionName = Lens.lens (\CloudWatchDimensionConfiguration' {dimensionName} -> dimensionName) (\s@CloudWatchDimensionConfiguration' {} a -> s {dimensionName = a} :: CloudWatchDimensionConfiguration)

-- | The location where the Amazon SES API v2 finds the value of a dimension
-- to publish to Amazon CloudWatch. To use the message tags that you
-- specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the
-- @SendEmail@ or @SendRawEmail@ API, choose @messageTag@. To use your own
-- email headers, choose @emailHeader@. To use link tags, choose
-- @linkTags@.
cloudWatchDimensionConfiguration_dimensionValueSource :: Lens.Lens' CloudWatchDimensionConfiguration DimensionValueSource
cloudWatchDimensionConfiguration_dimensionValueSource = Lens.lens (\CloudWatchDimensionConfiguration' {dimensionValueSource} -> dimensionValueSource) (\s@CloudWatchDimensionConfiguration' {} a -> s {dimensionValueSource = a} :: CloudWatchDimensionConfiguration)

-- | The default value of the dimension that is published to Amazon
-- CloudWatch if you don\'t provide the value of the dimension when you
-- send an email. This value has to meet the following criteria:
--
-- -   Can only contain ASCII letters (a–z, A–Z), numbers (0–9),
--     underscores (_), or dashes (-), at signs (\@), and periods (.).
--
-- -   It can contain no more than 256 characters.
cloudWatchDimensionConfiguration_defaultDimensionValue :: Lens.Lens' CloudWatchDimensionConfiguration Prelude.Text
cloudWatchDimensionConfiguration_defaultDimensionValue = Lens.lens (\CloudWatchDimensionConfiguration' {defaultDimensionValue} -> defaultDimensionValue) (\s@CloudWatchDimensionConfiguration' {} a -> s {defaultDimensionValue = a} :: CloudWatchDimensionConfiguration)

instance
  Core.FromJSON
    CloudWatchDimensionConfiguration
  where
  parseJSON =
    Core.withObject
      "CloudWatchDimensionConfiguration"
      ( \x ->
          CloudWatchDimensionConfiguration'
            Prelude.<$> (x Core..: "DimensionName")
            Prelude.<*> (x Core..: "DimensionValueSource")
            Prelude.<*> (x Core..: "DefaultDimensionValue")
      )

instance
  Prelude.Hashable
    CloudWatchDimensionConfiguration
  where
  hashWithSalt
    _salt
    CloudWatchDimensionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` dimensionName
        `Prelude.hashWithSalt` dimensionValueSource
        `Prelude.hashWithSalt` defaultDimensionValue

instance
  Prelude.NFData
    CloudWatchDimensionConfiguration
  where
  rnf CloudWatchDimensionConfiguration' {..} =
    Prelude.rnf dimensionName
      `Prelude.seq` Prelude.rnf dimensionValueSource
      `Prelude.seq` Prelude.rnf defaultDimensionValue

instance Core.ToJSON CloudWatchDimensionConfiguration where
  toJSON CloudWatchDimensionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DimensionName" Core..= dimensionName),
            Prelude.Just
              ( "DimensionValueSource"
                  Core..= dimensionValueSource
              ),
            Prelude.Just
              ( "DefaultDimensionValue"
                  Core..= defaultDimensionValue
              )
          ]
      )
