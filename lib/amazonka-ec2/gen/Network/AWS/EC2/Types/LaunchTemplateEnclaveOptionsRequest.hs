{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
--
--
-- /See:/ 'launchTemplateEnclaveOptionsRequest' smart constructor.
newtype LaunchTemplateEnclaveOptionsRequest = LaunchTemplateEnclaveOptionsRequest'
  { _lteorEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateEnclaveOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lteorEnabled' - To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
launchTemplateEnclaveOptionsRequest ::
  LaunchTemplateEnclaveOptionsRequest
launchTemplateEnclaveOptionsRequest =
  LaunchTemplateEnclaveOptionsRequest' {_lteorEnabled = Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
lteorEnabled :: Lens' LaunchTemplateEnclaveOptionsRequest (Maybe Bool)
lteorEnabled = lens _lteorEnabled (\s a -> s {_lteorEnabled = a})

instance Hashable LaunchTemplateEnclaveOptionsRequest

instance NFData LaunchTemplateEnclaveOptionsRequest

instance ToQuery LaunchTemplateEnclaveOptionsRequest where
  toQuery LaunchTemplateEnclaveOptionsRequest' {..} =
    mconcat ["Enabled" =: _lteorEnabled]
