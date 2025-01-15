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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataDetails

-- | Specifies the properties for creating an Amazon Elastic Compute Cloud
-- (Amazon EC2) launch template.
--
-- /See:/ 'newAwsEc2LaunchTemplateDetails' smart constructor.
data AwsEc2LaunchTemplateDetails = AwsEc2LaunchTemplateDetails'
  { -- | The default version of the launch template.
    defaultVersionNumber :: Prelude.Maybe Prelude.Integer,
    -- | An ID for the launch template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the launch template.
    latestVersionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The information to include in the launch template.
    launchTemplateData :: Prelude.Maybe AwsEc2LaunchTemplateDataDetails,
    -- | A name for the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersionNumber', 'awsEc2LaunchTemplateDetails_defaultVersionNumber' - The default version of the launch template.
--
-- 'id', 'awsEc2LaunchTemplateDetails_id' - An ID for the launch template.
--
-- 'latestVersionNumber', 'awsEc2LaunchTemplateDetails_latestVersionNumber' - The latest version of the launch template.
--
-- 'launchTemplateData', 'awsEc2LaunchTemplateDetails_launchTemplateData' - The information to include in the launch template.
--
-- 'launchTemplateName', 'awsEc2LaunchTemplateDetails_launchTemplateName' - A name for the launch template.
newAwsEc2LaunchTemplateDetails ::
  AwsEc2LaunchTemplateDetails
newAwsEc2LaunchTemplateDetails =
  AwsEc2LaunchTemplateDetails'
    { defaultVersionNumber =
        Prelude.Nothing,
      id = Prelude.Nothing,
      latestVersionNumber = Prelude.Nothing,
      launchTemplateData = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing
    }

-- | The default version of the launch template.
awsEc2LaunchTemplateDetails_defaultVersionNumber :: Lens.Lens' AwsEc2LaunchTemplateDetails (Prelude.Maybe Prelude.Integer)
awsEc2LaunchTemplateDetails_defaultVersionNumber = Lens.lens (\AwsEc2LaunchTemplateDetails' {defaultVersionNumber} -> defaultVersionNumber) (\s@AwsEc2LaunchTemplateDetails' {} a -> s {defaultVersionNumber = a} :: AwsEc2LaunchTemplateDetails)

-- | An ID for the launch template.
awsEc2LaunchTemplateDetails_id :: Lens.Lens' AwsEc2LaunchTemplateDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDetails_id = Lens.lens (\AwsEc2LaunchTemplateDetails' {id} -> id) (\s@AwsEc2LaunchTemplateDetails' {} a -> s {id = a} :: AwsEc2LaunchTemplateDetails)

-- | The latest version of the launch template.
awsEc2LaunchTemplateDetails_latestVersionNumber :: Lens.Lens' AwsEc2LaunchTemplateDetails (Prelude.Maybe Prelude.Integer)
awsEc2LaunchTemplateDetails_latestVersionNumber = Lens.lens (\AwsEc2LaunchTemplateDetails' {latestVersionNumber} -> latestVersionNumber) (\s@AwsEc2LaunchTemplateDetails' {} a -> s {latestVersionNumber = a} :: AwsEc2LaunchTemplateDetails)

-- | The information to include in the launch template.
awsEc2LaunchTemplateDetails_launchTemplateData :: Lens.Lens' AwsEc2LaunchTemplateDetails (Prelude.Maybe AwsEc2LaunchTemplateDataDetails)
awsEc2LaunchTemplateDetails_launchTemplateData = Lens.lens (\AwsEc2LaunchTemplateDetails' {launchTemplateData} -> launchTemplateData) (\s@AwsEc2LaunchTemplateDetails' {} a -> s {launchTemplateData = a} :: AwsEc2LaunchTemplateDetails)

-- | A name for the launch template.
awsEc2LaunchTemplateDetails_launchTemplateName :: Lens.Lens' AwsEc2LaunchTemplateDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDetails_launchTemplateName = Lens.lens (\AwsEc2LaunchTemplateDetails' {launchTemplateName} -> launchTemplateName) (\s@AwsEc2LaunchTemplateDetails' {} a -> s {launchTemplateName = a} :: AwsEc2LaunchTemplateDetails)

instance Data.FromJSON AwsEc2LaunchTemplateDetails where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDetails"
      ( \x ->
          AwsEc2LaunchTemplateDetails'
            Prelude.<$> (x Data..:? "DefaultVersionNumber")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LatestVersionNumber")
            Prelude.<*> (x Data..:? "LaunchTemplateData")
            Prelude.<*> (x Data..:? "LaunchTemplateName")
      )

instance Prelude.Hashable AwsEc2LaunchTemplateDetails where
  hashWithSalt _salt AwsEc2LaunchTemplateDetails' {..} =
    _salt
      `Prelude.hashWithSalt` defaultVersionNumber
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` latestVersionNumber
      `Prelude.hashWithSalt` launchTemplateData
      `Prelude.hashWithSalt` launchTemplateName

instance Prelude.NFData AwsEc2LaunchTemplateDetails where
  rnf AwsEc2LaunchTemplateDetails' {..} =
    Prelude.rnf defaultVersionNumber `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf latestVersionNumber `Prelude.seq`
          Prelude.rnf launchTemplateData `Prelude.seq`
            Prelude.rnf launchTemplateName

instance Data.ToJSON AwsEc2LaunchTemplateDetails where
  toJSON AwsEc2LaunchTemplateDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultVersionNumber" Data..=)
              Prelude.<$> defaultVersionNumber,
            ("Id" Data..=) Prelude.<$> id,
            ("LatestVersionNumber" Data..=)
              Prelude.<$> latestVersionNumber,
            ("LaunchTemplateData" Data..=)
              Prelude.<$> launchTemplateData,
            ("LaunchTemplateName" Data..=)
              Prelude.<$> launchTemplateName
          ]
      )
