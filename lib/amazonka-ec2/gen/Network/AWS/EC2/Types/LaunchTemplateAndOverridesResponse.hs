{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template and overrides.
--
--
--
-- /See:/ 'launchTemplateAndOverridesResponse' smart constructor.
data LaunchTemplateAndOverridesResponse = LaunchTemplateAndOverridesResponse'
  { _ltaoOverrides ::
      !( Maybe
           FleetLaunchTemplateOverrides
       ),
    _ltaoLaunchTemplateSpecification ::
      !( Maybe
           FleetLaunchTemplateSpecification
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateAndOverridesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltaoOverrides' - Any parameters that you specify override the same parameters in the launch template.
--
-- * 'ltaoLaunchTemplateSpecification' - The launch template.
launchTemplateAndOverridesResponse ::
  LaunchTemplateAndOverridesResponse
launchTemplateAndOverridesResponse =
  LaunchTemplateAndOverridesResponse'
    { _ltaoOverrides = Nothing,
      _ltaoLaunchTemplateSpecification = Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
ltaoOverrides :: Lens' LaunchTemplateAndOverridesResponse (Maybe FleetLaunchTemplateOverrides)
ltaoOverrides = lens _ltaoOverrides (\s a -> s {_ltaoOverrides = a})

-- | The launch template.
ltaoLaunchTemplateSpecification :: Lens' LaunchTemplateAndOverridesResponse (Maybe FleetLaunchTemplateSpecification)
ltaoLaunchTemplateSpecification = lens _ltaoLaunchTemplateSpecification (\s a -> s {_ltaoLaunchTemplateSpecification = a})

instance FromXML LaunchTemplateAndOverridesResponse where
  parseXML x =
    LaunchTemplateAndOverridesResponse'
      <$> (x .@? "overrides") <*> (x .@? "launchTemplateSpecification")

instance Hashable LaunchTemplateAndOverridesResponse

instance NFData LaunchTemplateAndOverridesResponse
