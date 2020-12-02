{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.EC2.Types.LaunchTemplateOverrides
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template and overrides.
--
--
--
-- /See:/ 'launchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { _ltcOverrides ::
      !(Maybe [LaunchTemplateOverrides]),
    _ltcLaunchTemplateSpecification ::
      !(Maybe FleetLaunchTemplateSpecification)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcOverrides' - Any parameters that you specify override the same parameters in the launch template.
--
-- * 'ltcLaunchTemplateSpecification' - The launch template.
launchTemplateConfig ::
  LaunchTemplateConfig
launchTemplateConfig =
  LaunchTemplateConfig'
    { _ltcOverrides = Nothing,
      _ltcLaunchTemplateSpecification = Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
ltcOverrides :: Lens' LaunchTemplateConfig [LaunchTemplateOverrides]
ltcOverrides = lens _ltcOverrides (\s a -> s {_ltcOverrides = a}) . _Default . _Coerce

-- | The launch template.
ltcLaunchTemplateSpecification :: Lens' LaunchTemplateConfig (Maybe FleetLaunchTemplateSpecification)
ltcLaunchTemplateSpecification = lens _ltcLaunchTemplateSpecification (\s a -> s {_ltcLaunchTemplateSpecification = a})

instance FromXML LaunchTemplateConfig where
  parseXML x =
    LaunchTemplateConfig'
      <$> (x .@? "overrides" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "launchTemplateSpecification")

instance Hashable LaunchTemplateConfig

instance NFData LaunchTemplateConfig

instance ToQuery LaunchTemplateConfig where
  toQuery LaunchTemplateConfig' {..} =
    mconcat
      [ toQuery (toQueryList "Overrides" <$> _ltcOverrides),
        "LaunchTemplateSpecification" =: _ltcLaunchTemplateSpecification
      ]
