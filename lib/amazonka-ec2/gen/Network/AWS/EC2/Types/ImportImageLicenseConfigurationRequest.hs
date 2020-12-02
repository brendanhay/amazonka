{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The request information of license configurations.
--
--
--
-- /See:/ 'importImageLicenseConfigurationRequest' smart constructor.
newtype ImportImageLicenseConfigurationRequest = ImportImageLicenseConfigurationRequest'
  { _iilcrLicenseConfigurationARN ::
      Maybe Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ImportImageLicenseConfigurationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iilcrLicenseConfigurationARN' - The ARN of a license configuration.
importImageLicenseConfigurationRequest ::
  ImportImageLicenseConfigurationRequest
importImageLicenseConfigurationRequest =
  ImportImageLicenseConfigurationRequest'
    { _iilcrLicenseConfigurationARN =
        Nothing
    }

-- | The ARN of a license configuration.
iilcrLicenseConfigurationARN :: Lens' ImportImageLicenseConfigurationRequest (Maybe Text)
iilcrLicenseConfigurationARN = lens _iilcrLicenseConfigurationARN (\s a -> s {_iilcrLicenseConfigurationARN = a})

instance Hashable ImportImageLicenseConfigurationRequest

instance NFData ImportImageLicenseConfigurationRequest

instance ToQuery ImportImageLicenseConfigurationRequest where
  toQuery ImportImageLicenseConfigurationRequest' {..} =
    mconcat
      ["LicenseConfigurationArn" =: _iilcrLicenseConfigurationARN]
