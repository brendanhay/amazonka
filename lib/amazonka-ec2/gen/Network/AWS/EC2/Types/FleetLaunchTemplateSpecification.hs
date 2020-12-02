{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by a Spot Fleet request to configure Amazon EC2 instances. For information about launch templates, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
--
-- /See:/ 'fleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { _fltsLaunchTemplateName ::
      !(Maybe Text),
    _fltsLaunchTemplateId ::
      !(Maybe Text),
    _fltsVersion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetLaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltsLaunchTemplateName' - The name of the launch template. If you specify the template name, you can't specify the template ID.
--
-- * 'fltsLaunchTemplateId' - The ID of the launch template. If you specify the template ID, you can't specify the template name.
--
-- * 'fltsVersion' - The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails. If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template. If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
fleetLaunchTemplateSpecification ::
  FleetLaunchTemplateSpecification
fleetLaunchTemplateSpecification =
  FleetLaunchTemplateSpecification'
    { _fltsLaunchTemplateName =
        Nothing,
      _fltsLaunchTemplateId = Nothing,
      _fltsVersion = Nothing
    }

-- | The name of the launch template. If you specify the template name, you can't specify the template ID.
fltsLaunchTemplateName :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsLaunchTemplateName = lens _fltsLaunchTemplateName (\s a -> s {_fltsLaunchTemplateName = a})

-- | The ID of the launch template. If you specify the template ID, you can't specify the template name.
fltsLaunchTemplateId :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsLaunchTemplateId = lens _fltsLaunchTemplateId (\s a -> s {_fltsLaunchTemplateId = a})

-- | The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails. If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template. If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
fltsVersion :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsVersion = lens _fltsVersion (\s a -> s {_fltsVersion = a})

instance FromXML FleetLaunchTemplateSpecification where
  parseXML x =
    FleetLaunchTemplateSpecification'
      <$> (x .@? "launchTemplateName")
      <*> (x .@? "launchTemplateId")
      <*> (x .@? "version")

instance Hashable FleetLaunchTemplateSpecification

instance NFData FleetLaunchTemplateSpecification

instance ToQuery FleetLaunchTemplateSpecification where
  toQuery FleetLaunchTemplateSpecification' {..} =
    mconcat
      [ "LaunchTemplateName" =: _fltsLaunchTemplateName,
        "LaunchTemplateId" =: _fltsLaunchTemplateId,
        "Version" =: _fltsVersion
      ]
