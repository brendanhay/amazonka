{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a license configuration.
--
--
--
-- /See:/ 'launchTemplateLicenseConfigurationRequest' smart constructor.
newtype LaunchTemplateLicenseConfigurationRequest = LaunchTemplateLicenseConfigurationRequest'
  { _ltlcrLicenseConfigurationARN ::
      Maybe
        Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateLicenseConfigurationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlcrLicenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
launchTemplateLicenseConfigurationRequest ::
  LaunchTemplateLicenseConfigurationRequest
launchTemplateLicenseConfigurationRequest =
  LaunchTemplateLicenseConfigurationRequest'
    { _ltlcrLicenseConfigurationARN =
        Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
ltlcrLicenseConfigurationARN :: Lens' LaunchTemplateLicenseConfigurationRequest (Maybe Text)
ltlcrLicenseConfigurationARN = lens _ltlcrLicenseConfigurationARN (\s a -> s {_ltlcrLicenseConfigurationARN = a})

instance Hashable LaunchTemplateLicenseConfigurationRequest

instance NFData LaunchTemplateLicenseConfigurationRequest

instance ToQuery LaunchTemplateLicenseConfigurationRequest where
  toQuery LaunchTemplateLicenseConfigurationRequest' {..} =
    mconcat
      ["LicenseConfigurationArn" =: _ltlcrLicenseConfigurationARN]
