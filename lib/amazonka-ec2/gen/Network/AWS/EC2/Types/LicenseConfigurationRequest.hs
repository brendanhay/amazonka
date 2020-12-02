{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LicenseConfigurationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a license configuration.
--
--
--
-- /See:/ 'licenseConfigurationRequest' smart constructor.
newtype LicenseConfigurationRequest = LicenseConfigurationRequest'
  { _lcrLicenseConfigurationARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LicenseConfigurationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrLicenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
licenseConfigurationRequest ::
  LicenseConfigurationRequest
licenseConfigurationRequest =
  LicenseConfigurationRequest'
    { _lcrLicenseConfigurationARN =
        Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
lcrLicenseConfigurationARN :: Lens' LicenseConfigurationRequest (Maybe Text)
lcrLicenseConfigurationARN = lens _lcrLicenseConfigurationARN (\s a -> s {_lcrLicenseConfigurationARN = a})

instance Hashable LicenseConfigurationRequest

instance NFData LicenseConfigurationRequest

instance ToQuery LicenseConfigurationRequest where
  toQuery LicenseConfigurationRequest' {..} =
    mconcat
      ["LicenseConfigurationArn" =: _lcrLicenseConfigurationARN]
