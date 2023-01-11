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
-- Module      : Amazonka.Inspector2.Types.ResourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.AwsEc2InstanceDetails
import Amazonka.Inspector2.Types.AwsEcrContainerImageDetails
import Amazonka.Inspector2.Types.AwsLambdaFunctionDetails
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the resource involved in the finding.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | An object that contains details about the Amazon EC2 instance involved
    -- in the finding.
    awsEc2Instance :: Prelude.Maybe AwsEc2InstanceDetails,
    -- | An object that contains details about the Amazon ECR container image
    -- involved in the finding.
    awsEcrContainerImage :: Prelude.Maybe AwsEcrContainerImageDetails,
    -- | A summary of the information about an AWS Lambda function affected by a
    -- finding.
    awsLambdaFunction :: Prelude.Maybe AwsLambdaFunctionDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsEc2Instance', 'resourceDetails_awsEc2Instance' - An object that contains details about the Amazon EC2 instance involved
-- in the finding.
--
-- 'awsEcrContainerImage', 'resourceDetails_awsEcrContainerImage' - An object that contains details about the Amazon ECR container image
-- involved in the finding.
--
-- 'awsLambdaFunction', 'resourceDetails_awsLambdaFunction' - A summary of the information about an AWS Lambda function affected by a
-- finding.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { awsEc2Instance = Prelude.Nothing,
      awsEcrContainerImage = Prelude.Nothing,
      awsLambdaFunction = Prelude.Nothing
    }

-- | An object that contains details about the Amazon EC2 instance involved
-- in the finding.
resourceDetails_awsEc2Instance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2InstanceDetails)
resourceDetails_awsEc2Instance = Lens.lens (\ResourceDetails' {awsEc2Instance} -> awsEc2Instance) (\s@ResourceDetails' {} a -> s {awsEc2Instance = a} :: ResourceDetails)

-- | An object that contains details about the Amazon ECR container image
-- involved in the finding.
resourceDetails_awsEcrContainerImage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcrContainerImageDetails)
resourceDetails_awsEcrContainerImage = Lens.lens (\ResourceDetails' {awsEcrContainerImage} -> awsEcrContainerImage) (\s@ResourceDetails' {} a -> s {awsEcrContainerImage = a} :: ResourceDetails)

-- | A summary of the information about an AWS Lambda function affected by a
-- finding.
resourceDetails_awsLambdaFunction :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsLambdaFunctionDetails)
resourceDetails_awsLambdaFunction = Lens.lens (\ResourceDetails' {awsLambdaFunction} -> awsLambdaFunction) (\s@ResourceDetails' {} a -> s {awsLambdaFunction = a} :: ResourceDetails)

instance Data.FromJSON ResourceDetails where
  parseJSON =
    Data.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Data..:? "awsEc2Instance")
            Prelude.<*> (x Data..:? "awsEcrContainerImage")
            Prelude.<*> (x Data..:? "awsLambdaFunction")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt `Prelude.hashWithSalt` awsEc2Instance
      `Prelude.hashWithSalt` awsEcrContainerImage
      `Prelude.hashWithSalt` awsLambdaFunction

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf awsEc2Instance
      `Prelude.seq` Prelude.rnf awsEcrContainerImage
      `Prelude.seq` Prelude.rnf awsLambdaFunction
