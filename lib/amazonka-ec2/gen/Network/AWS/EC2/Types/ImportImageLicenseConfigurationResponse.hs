{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The response information for license configurations.
--
--
--
-- /See:/ 'importImageLicenseConfigurationResponse' smart constructor.
newtype ImportImageLicenseConfigurationResponse = ImportImageLicenseConfigurationResponse'
  { _iilcLicenseConfigurationARN ::
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

-- | Creates a value of 'ImportImageLicenseConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iilcLicenseConfigurationARN' - The ARN of a license configuration.
importImageLicenseConfigurationResponse ::
  ImportImageLicenseConfigurationResponse
importImageLicenseConfigurationResponse =
  ImportImageLicenseConfigurationResponse'
    { _iilcLicenseConfigurationARN =
        Nothing
    }

-- | The ARN of a license configuration.
iilcLicenseConfigurationARN :: Lens' ImportImageLicenseConfigurationResponse (Maybe Text)
iilcLicenseConfigurationARN = lens _iilcLicenseConfigurationARN (\s a -> s {_iilcLicenseConfigurationARN = a})

instance FromXML ImportImageLicenseConfigurationResponse where
  parseXML x =
    ImportImageLicenseConfigurationResponse'
      <$> (x .@? "licenseConfigurationArn")

instance Hashable ImportImageLicenseConfigurationResponse

instance NFData ImportImageLicenseConfigurationResponse
