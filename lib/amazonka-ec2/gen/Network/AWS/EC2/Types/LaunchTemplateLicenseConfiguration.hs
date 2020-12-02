{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a license configuration.
--
--
--
-- /See:/ 'launchTemplateLicenseConfiguration' smart constructor.
newtype LaunchTemplateLicenseConfiguration = LaunchTemplateLicenseConfiguration'
  { _ltlcLicenseConfigurationARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateLicenseConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlcLicenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
launchTemplateLicenseConfiguration ::
  LaunchTemplateLicenseConfiguration
launchTemplateLicenseConfiguration =
  LaunchTemplateLicenseConfiguration'
    { _ltlcLicenseConfigurationARN =
        Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
ltlcLicenseConfigurationARN :: Lens' LaunchTemplateLicenseConfiguration (Maybe Text)
ltlcLicenseConfigurationARN = lens _ltlcLicenseConfigurationARN (\s a -> s {_ltlcLicenseConfigurationARN = a})

instance FromXML LaunchTemplateLicenseConfiguration where
  parseXML x =
    LaunchTemplateLicenseConfiguration'
      <$> (x .@? "licenseConfigurationArn")

instance Hashable LaunchTemplateLicenseConfiguration

instance NFData LaunchTemplateLicenseConfiguration
