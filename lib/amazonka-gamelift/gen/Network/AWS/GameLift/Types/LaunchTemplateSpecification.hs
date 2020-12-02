{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.LaunchTemplateSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
--
-- An EC2 launch template that contains configuration settings and game server code to be deployed to all instances in a game server group. The launch template is specified when creating a new game server group with 'CreateGameServerGroup' .
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
-- * 'ltsLaunchTemplateName' - A readable identifier for an existing EC2 launch template.
--
-- * 'ltsLaunchTemplateId' - A unique identifier for an existing EC2 launch template.
--
-- * 'ltsVersion' - The version of the EC2 launch template to use. If no version is specified, the default version will be used. With Amazon EC2, you can specify a default version for a launch template. If none is set, the default is the first version created.
launchTemplateSpecification ::
  LaunchTemplateSpecification
launchTemplateSpecification =
  LaunchTemplateSpecification'
    { _ltsLaunchTemplateName = Nothing,
      _ltsLaunchTemplateId = Nothing,
      _ltsVersion = Nothing
    }

-- | A readable identifier for an existing EC2 launch template.
ltsLaunchTemplateName :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateName = lens _ltsLaunchTemplateName (\s a -> s {_ltsLaunchTemplateName = a})

-- | A unique identifier for an existing EC2 launch template.
ltsLaunchTemplateId :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateId = lens _ltsLaunchTemplateId (\s a -> s {_ltsLaunchTemplateId = a})

-- | The version of the EC2 launch template to use. If no version is specified, the default version will be used. With Amazon EC2, you can specify a default version for a launch template. If none is set, the default is the first version created.
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\s a -> s {_ltsVersion = a})

instance Hashable LaunchTemplateSpecification

instance NFData LaunchTemplateSpecification

instance ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    object
      ( catMaybes
          [ ("LaunchTemplateName" .=) <$> _ltsLaunchTemplateName,
            ("LaunchTemplateId" .=) <$> _ltsLaunchTemplateId,
            ("Version" .=) <$> _ltsVersion
          ]
      )
