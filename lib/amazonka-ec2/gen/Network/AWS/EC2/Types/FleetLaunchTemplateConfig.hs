{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template and overrides.
--
--
--
-- /See:/ 'fleetLaunchTemplateConfig' smart constructor.
data FleetLaunchTemplateConfig = FleetLaunchTemplateConfig'
  { _fltcOverrides ::
      !(Maybe [FleetLaunchTemplateOverrides]),
    _fltcLaunchTemplateSpecification ::
      !( Maybe
           FleetLaunchTemplateSpecification
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetLaunchTemplateConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltcOverrides' - Any parameters that you specify override the same parameters in the launch template.
--
-- * 'fltcLaunchTemplateSpecification' - The launch template.
fleetLaunchTemplateConfig ::
  FleetLaunchTemplateConfig
fleetLaunchTemplateConfig =
  FleetLaunchTemplateConfig'
    { _fltcOverrides = Nothing,
      _fltcLaunchTemplateSpecification = Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
fltcOverrides :: Lens' FleetLaunchTemplateConfig [FleetLaunchTemplateOverrides]
fltcOverrides = lens _fltcOverrides (\s a -> s {_fltcOverrides = a}) . _Default . _Coerce

-- | The launch template.
fltcLaunchTemplateSpecification :: Lens' FleetLaunchTemplateConfig (Maybe FleetLaunchTemplateSpecification)
fltcLaunchTemplateSpecification = lens _fltcLaunchTemplateSpecification (\s a -> s {_fltcLaunchTemplateSpecification = a})

instance FromXML FleetLaunchTemplateConfig where
  parseXML x =
    FleetLaunchTemplateConfig'
      <$> (x .@? "overrides" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "launchTemplateSpecification")

instance Hashable FleetLaunchTemplateConfig

instance NFData FleetLaunchTemplateConfig
