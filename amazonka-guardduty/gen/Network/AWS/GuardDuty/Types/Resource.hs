{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Resource where

import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.S3BucketDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the AWS resource associated with the activity
-- that prompted GuardDuty to generate a finding.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | Contains information on the S3 bucket.
    s3BucketDetails :: Prelude.Maybe [S3BucketDetail],
    -- | The information about the EC2 instance associated with the activity that
    -- prompted GuardDuty to generate a finding.
    instanceDetails :: Prelude.Maybe InstanceDetails,
    -- | The type of AWS resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The IAM access key details (IAM user information) of a user that engaged
    -- in the activity that prompted GuardDuty to generate a finding.
    accessKeyDetails :: Prelude.Maybe AccessKeyDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketDetails', 'resource_s3BucketDetails' - Contains information on the S3 bucket.
--
-- 'instanceDetails', 'resource_instanceDetails' - The information about the EC2 instance associated with the activity that
-- prompted GuardDuty to generate a finding.
--
-- 'resourceType', 'resource_resourceType' - The type of AWS resource.
--
-- 'accessKeyDetails', 'resource_accessKeyDetails' - The IAM access key details (IAM user information) of a user that engaged
-- in the activity that prompted GuardDuty to generate a finding.
newResource ::
  Resource
newResource =
  Resource'
    { s3BucketDetails = Prelude.Nothing,
      instanceDetails = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      accessKeyDetails = Prelude.Nothing
    }

-- | Contains information on the S3 bucket.
resource_s3BucketDetails :: Lens.Lens' Resource (Prelude.Maybe [S3BucketDetail])
resource_s3BucketDetails = Lens.lens (\Resource' {s3BucketDetails} -> s3BucketDetails) (\s@Resource' {} a -> s {s3BucketDetails = a} :: Resource) Prelude.. Lens.mapping Prelude._Coerce

-- | The information about the EC2 instance associated with the activity that
-- prompted GuardDuty to generate a finding.
resource_instanceDetails :: Lens.Lens' Resource (Prelude.Maybe InstanceDetails)
resource_instanceDetails = Lens.lens (\Resource' {instanceDetails} -> instanceDetails) (\s@Resource' {} a -> s {instanceDetails = a} :: Resource)

-- | The type of AWS resource.
resource_resourceType :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceType = Lens.lens (\Resource' {resourceType} -> resourceType) (\s@Resource' {} a -> s {resourceType = a} :: Resource)

-- | The IAM access key details (IAM user information) of a user that engaged
-- in the activity that prompted GuardDuty to generate a finding.
resource_accessKeyDetails :: Lens.Lens' Resource (Prelude.Maybe AccessKeyDetails)
resource_accessKeyDetails = Lens.lens (\Resource' {accessKeyDetails} -> accessKeyDetails) (\s@Resource' {} a -> s {accessKeyDetails = a} :: Resource)

instance Prelude.FromJSON Resource where
  parseJSON =
    Prelude.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> ( x Prelude..:? "s3BucketDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "instanceDetails")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "accessKeyDetails")
      )

instance Prelude.Hashable Resource

instance Prelude.NFData Resource
