{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Resource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the type and name of a resource referenced by an event.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceType :: !(Maybe Text),
    _rResourceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceType' - The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. To learn more about how to look up and filter events by the resource types supported for a service, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events> .
--
-- * 'rResourceName' - The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
resource ::
  Resource
resource =
  Resource' {_rResourceType = Nothing, _rResourceName = Nothing}

-- | The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. To learn more about how to look up and filter events by the resource types supported for a service, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events> .
rResourceType :: Lens' Resource (Maybe Text)
rResourceType = lens _rResourceType (\s a -> s {_rResourceType = a})

-- | The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
rResourceName :: Lens' Resource (Maybe Text)
rResourceName = lens _rResourceName (\s a -> s {_rResourceName = a})

instance FromJSON Resource where
  parseJSON =
    withObject
      "Resource"
      ( \x ->
          Resource' <$> (x .:? "ResourceType") <*> (x .:? "ResourceName")
      )

instance Hashable Resource

instance NFData Resource
