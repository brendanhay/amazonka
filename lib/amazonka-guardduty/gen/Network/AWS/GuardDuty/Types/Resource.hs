{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Resource where

import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.S3BucketDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the AWS resource associated with the activity that prompted GuardDuty to generate a finding.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceType :: !(Maybe Text),
    _rS3BucketDetails :: !(Maybe [S3BucketDetail]),
    _rInstanceDetails :: !(Maybe InstanceDetails),
    _rAccessKeyDetails :: !(Maybe AccessKeyDetails)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceType' - The type of AWS resource.
--
-- * 'rS3BucketDetails' - Contains information on the S3 bucket.
--
-- * 'rInstanceDetails' - The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
--
-- * 'rAccessKeyDetails' - The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
resource ::
  Resource
resource =
  Resource'
    { _rResourceType = Nothing,
      _rS3BucketDetails = Nothing,
      _rInstanceDetails = Nothing,
      _rAccessKeyDetails = Nothing
    }

-- | The type of AWS resource.
rResourceType :: Lens' Resource (Maybe Text)
rResourceType = lens _rResourceType (\s a -> s {_rResourceType = a})

-- | Contains information on the S3 bucket.
rS3BucketDetails :: Lens' Resource [S3BucketDetail]
rS3BucketDetails = lens _rS3BucketDetails (\s a -> s {_rS3BucketDetails = a}) . _Default . _Coerce

-- | The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
rInstanceDetails :: Lens' Resource (Maybe InstanceDetails)
rInstanceDetails = lens _rInstanceDetails (\s a -> s {_rInstanceDetails = a})

-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
rAccessKeyDetails :: Lens' Resource (Maybe AccessKeyDetails)
rAccessKeyDetails = lens _rAccessKeyDetails (\s a -> s {_rAccessKeyDetails = a})

instance FromJSON Resource where
  parseJSON =
    withObject
      "Resource"
      ( \x ->
          Resource'
            <$> (x .:? "resourceType")
            <*> (x .:? "s3BucketDetails" .!= mempty)
            <*> (x .:? "instanceDetails")
            <*> (x .:? "accessKeyDetails")
      )

instance Hashable Resource

instance NFData Resource
