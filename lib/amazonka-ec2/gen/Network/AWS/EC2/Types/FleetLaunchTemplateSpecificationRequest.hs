{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by an EC2 Fleet to configure Amazon EC2 instances. For information about launch templates, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'fleetLaunchTemplateSpecificationRequest' smart constructor.
data FleetLaunchTemplateSpecificationRequest = FleetLaunchTemplateSpecificationRequest'
  { _fltsrLaunchTemplateName ::
      !( Maybe
           Text
       ),
    _fltsrLaunchTemplateId ::
      !( Maybe
           Text
       ),
    _fltsrVersion ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetLaunchTemplateSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltsrLaunchTemplateName' - The name of the launch template. If you specify the template name, you can't specify the template ID.
--
-- * 'fltsrLaunchTemplateId' - The ID of the launch template. If you specify the template ID, you can't specify the template name.
--
-- * 'fltsrVersion' - The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails. If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template. If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
fleetLaunchTemplateSpecificationRequest ::
  FleetLaunchTemplateSpecificationRequest
fleetLaunchTemplateSpecificationRequest =
  FleetLaunchTemplateSpecificationRequest'
    { _fltsrLaunchTemplateName =
        Nothing,
      _fltsrLaunchTemplateId = Nothing,
      _fltsrVersion = Nothing
    }

-- | The name of the launch template. If you specify the template name, you can't specify the template ID.
fltsrLaunchTemplateName :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrLaunchTemplateName = lens _fltsrLaunchTemplateName (\s a -> s {_fltsrLaunchTemplateName = a})

-- | The ID of the launch template. If you specify the template ID, you can't specify the template name.
fltsrLaunchTemplateId :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrLaunchTemplateId = lens _fltsrLaunchTemplateId (\s a -> s {_fltsrLaunchTemplateId = a})

-- | The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails. If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template. If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
fltsrVersion :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrVersion = lens _fltsrVersion (\s a -> s {_fltsrVersion = a})

instance Hashable FleetLaunchTemplateSpecificationRequest

instance NFData FleetLaunchTemplateSpecificationRequest

instance ToQuery FleetLaunchTemplateSpecificationRequest where
  toQuery FleetLaunchTemplateSpecificationRequest' {..} =
    mconcat
      [ "LaunchTemplateName" =: _fltsrLaunchTemplateName,
        "LaunchTemplateId" =: _fltsrLaunchTemplateId,
        "Version" =: _fltsrVersion
      ]
