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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataIamInstanceProfileDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataIamInstanceProfileDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details for an Identity and Access Management (IAM) instance
-- profile, which is a container for an IAM role for your instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataIamInstanceProfileDetails' smart constructor.
data AwsEc2LaunchTemplateDataIamInstanceProfileDetails = AwsEc2LaunchTemplateDataIamInstanceProfileDetails'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance profile.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataIamInstanceProfileDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsEc2LaunchTemplateDataIamInstanceProfileDetails_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'awsEc2LaunchTemplateDataIamInstanceProfileDetails_name' - The name of the instance profile.
newAwsEc2LaunchTemplateDataIamInstanceProfileDetails ::
  AwsEc2LaunchTemplateDataIamInstanceProfileDetails
newAwsEc2LaunchTemplateDataIamInstanceProfileDetails =
  AwsEc2LaunchTemplateDataIamInstanceProfileDetails'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
awsEc2LaunchTemplateDataIamInstanceProfileDetails_arn :: Lens.Lens' AwsEc2LaunchTemplateDataIamInstanceProfileDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataIamInstanceProfileDetails_arn = Lens.lens (\AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {arn} -> arn) (\s@AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {} a -> s {arn = a} :: AwsEc2LaunchTemplateDataIamInstanceProfileDetails)

-- | The name of the instance profile.
awsEc2LaunchTemplateDataIamInstanceProfileDetails_name :: Lens.Lens' AwsEc2LaunchTemplateDataIamInstanceProfileDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataIamInstanceProfileDetails_name = Lens.lens (\AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {name} -> name) (\s@AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {} a -> s {name = a} :: AwsEc2LaunchTemplateDataIamInstanceProfileDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataIamInstanceProfileDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataIamInstanceProfileDetails'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails
  where
  rnf
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {..} =
      Prelude.rnf arn `Prelude.seq` Prelude.rnf name

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              ("Name" Data..=) Prelude.<$> name
            ]
        )
