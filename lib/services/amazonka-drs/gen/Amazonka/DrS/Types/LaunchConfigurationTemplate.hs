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
-- Module      : Amazonka.DrS.Types.LaunchConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LaunchConfigurationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.LaunchDisposition
import Amazonka.DrS.Types.Licensing
import Amazonka.DrS.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude

-- | Account level Launch Configuration Template.
--
-- /See:/ 'newLaunchConfigurationTemplate' smart constructor.
data LaunchConfigurationTemplate = LaunchConfigurationTemplate'
  { -- | ARN of the Launch Configuration Template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Copy private IP.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Copy tags.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | S3 bucket ARN to export Source Network templates.
    exportBucketArn :: Prelude.Maybe Prelude.Text,
    -- | ID of the Launch Configuration Template.
    launchConfigurationTemplateID :: Prelude.Maybe Prelude.Text,
    -- | Launch disposition.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | Licensing.
    licensing :: Prelude.Maybe Licensing,
    -- | Tags of the Launch Configuration Template.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Target instance type right-sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'launchConfigurationTemplate_arn' - ARN of the Launch Configuration Template.
--
-- 'copyPrivateIp', 'launchConfigurationTemplate_copyPrivateIp' - Copy private IP.
--
-- 'copyTags', 'launchConfigurationTemplate_copyTags' - Copy tags.
--
-- 'exportBucketArn', 'launchConfigurationTemplate_exportBucketArn' - S3 bucket ARN to export Source Network templates.
--
-- 'launchConfigurationTemplateID', 'launchConfigurationTemplate_launchConfigurationTemplateID' - ID of the Launch Configuration Template.
--
-- 'launchDisposition', 'launchConfigurationTemplate_launchDisposition' - Launch disposition.
--
-- 'licensing', 'launchConfigurationTemplate_licensing' - Licensing.
--
-- 'tags', 'launchConfigurationTemplate_tags' - Tags of the Launch Configuration Template.
--
-- 'targetInstanceTypeRightSizingMethod', 'launchConfigurationTemplate_targetInstanceTypeRightSizingMethod' - Target instance type right-sizing method.
newLaunchConfigurationTemplate ::
  LaunchConfigurationTemplate
newLaunchConfigurationTemplate =
  LaunchConfigurationTemplate'
    { arn = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      exportBucketArn = Prelude.Nothing,
      launchConfigurationTemplateID =
        Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      licensing = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing
    }

-- | ARN of the Launch Configuration Template.
launchConfigurationTemplate_arn :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_arn = Lens.lens (\LaunchConfigurationTemplate' {arn} -> arn) (\s@LaunchConfigurationTemplate' {} a -> s {arn = a} :: LaunchConfigurationTemplate)

-- | Copy private IP.
launchConfigurationTemplate_copyPrivateIp :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
launchConfigurationTemplate_copyPrivateIp = Lens.lens (\LaunchConfigurationTemplate' {copyPrivateIp} -> copyPrivateIp) (\s@LaunchConfigurationTemplate' {} a -> s {copyPrivateIp = a} :: LaunchConfigurationTemplate)

-- | Copy tags.
launchConfigurationTemplate_copyTags :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
launchConfigurationTemplate_copyTags = Lens.lens (\LaunchConfigurationTemplate' {copyTags} -> copyTags) (\s@LaunchConfigurationTemplate' {} a -> s {copyTags = a} :: LaunchConfigurationTemplate)

-- | S3 bucket ARN to export Source Network templates.
launchConfigurationTemplate_exportBucketArn :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_exportBucketArn = Lens.lens (\LaunchConfigurationTemplate' {exportBucketArn} -> exportBucketArn) (\s@LaunchConfigurationTemplate' {} a -> s {exportBucketArn = a} :: LaunchConfigurationTemplate)

-- | ID of the Launch Configuration Template.
launchConfigurationTemplate_launchConfigurationTemplateID :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_launchConfigurationTemplateID = Lens.lens (\LaunchConfigurationTemplate' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@LaunchConfigurationTemplate' {} a -> s {launchConfigurationTemplateID = a} :: LaunchConfigurationTemplate)

-- | Launch disposition.
launchConfigurationTemplate_launchDisposition :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe LaunchDisposition)
launchConfigurationTemplate_launchDisposition = Lens.lens (\LaunchConfigurationTemplate' {launchDisposition} -> launchDisposition) (\s@LaunchConfigurationTemplate' {} a -> s {launchDisposition = a} :: LaunchConfigurationTemplate)

-- | Licensing.
launchConfigurationTemplate_licensing :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Licensing)
launchConfigurationTemplate_licensing = Lens.lens (\LaunchConfigurationTemplate' {licensing} -> licensing) (\s@LaunchConfigurationTemplate' {} a -> s {licensing = a} :: LaunchConfigurationTemplate)

-- | Tags of the Launch Configuration Template.
launchConfigurationTemplate_tags :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchConfigurationTemplate_tags = Lens.lens (\LaunchConfigurationTemplate' {tags} -> tags) (\s@LaunchConfigurationTemplate' {} a -> s {tags = a} :: LaunchConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Target instance type right-sizing method.
launchConfigurationTemplate_targetInstanceTypeRightSizingMethod :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
launchConfigurationTemplate_targetInstanceTypeRightSizingMethod = Lens.lens (\LaunchConfigurationTemplate' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@LaunchConfigurationTemplate' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: LaunchConfigurationTemplate)

instance Data.FromJSON LaunchConfigurationTemplate where
  parseJSON =
    Data.withObject
      "LaunchConfigurationTemplate"
      ( \x ->
          LaunchConfigurationTemplate'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "copyPrivateIp")
            Prelude.<*> (x Data..:? "copyTags")
            Prelude.<*> (x Data..:? "exportBucketArn")
            Prelude.<*> (x Data..:? "launchConfigurationTemplateID")
            Prelude.<*> (x Data..:? "launchDisposition")
            Prelude.<*> (x Data..:? "licensing")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targetInstanceTypeRightSizingMethod")
      )

instance Prelude.Hashable LaunchConfigurationTemplate where
  hashWithSalt _salt LaunchConfigurationTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` copyPrivateIp
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` exportBucketArn
      `Prelude.hashWithSalt` launchConfigurationTemplateID
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod

instance Prelude.NFData LaunchConfigurationTemplate where
  rnf LaunchConfigurationTemplate' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf exportBucketArn
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod
