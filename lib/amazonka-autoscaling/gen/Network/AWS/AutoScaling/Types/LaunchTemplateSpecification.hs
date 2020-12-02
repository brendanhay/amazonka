{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplateSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by an Auto Scaling group to configure Amazon EC2 instances.
--
--
-- The launch template that is specified must be configured for use with an Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
-- /See:/ 'launchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { _ltsLaunchTemplateName ::
      !(Maybe Text),
    _ltsLaunchTemplateId ::
      !(Maybe Text),
    _ltsVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsLaunchTemplateName' - The name of the launch template. To get the template name, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.  Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
--
-- * 'ltsLaunchTemplateId' - The ID of the launch template. To get the template ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.  Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
--
-- * 'ltsVersion' - The version number, @> Latest@ , or @> Default@ . To get the version number, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions> API operation. New launch template versions can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion> API. If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
launchTemplateSpecification ::
  LaunchTemplateSpecification
launchTemplateSpecification =
  LaunchTemplateSpecification'
    { _ltsLaunchTemplateName = Nothing,
      _ltsLaunchTemplateId = Nothing,
      _ltsVersion = Nothing
    }

-- | The name of the launch template. To get the template name, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.  Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
ltsLaunchTemplateName :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateName = lens _ltsLaunchTemplateName (\s a -> s {_ltsLaunchTemplateName = a})

-- | The ID of the launch template. To get the template ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.  Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
ltsLaunchTemplateId :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateId = lens _ltsLaunchTemplateId (\s a -> s {_ltsLaunchTemplateId = a})

-- | The version number, @> Latest@ , or @> Default@ . To get the version number, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions> API operation. New launch template versions can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion> API. If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\s a -> s {_ltsVersion = a})

instance FromXML LaunchTemplateSpecification where
  parseXML x =
    LaunchTemplateSpecification'
      <$> (x .@? "LaunchTemplateName")
      <*> (x .@? "LaunchTemplateId")
      <*> (x .@? "Version")

instance Hashable LaunchTemplateSpecification

instance NFData LaunchTemplateSpecification

instance ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    mconcat
      [ "LaunchTemplateName" =: _ltsLaunchTemplateName,
        "LaunchTemplateId" =: _ltsLaunchTemplateId,
        "Version" =: _ltsVersion
      ]
