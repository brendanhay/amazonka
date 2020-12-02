{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnclaveOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnclaveOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
--
--
-- /See:/ 'enclaveOptionsRequest' smart constructor.
newtype EnclaveOptionsRequest = EnclaveOptionsRequest'
  { _eorEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnclaveOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eorEnabled' - To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
enclaveOptionsRequest ::
  EnclaveOptionsRequest
enclaveOptionsRequest =
  EnclaveOptionsRequest' {_eorEnabled = Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
eorEnabled :: Lens' EnclaveOptionsRequest (Maybe Bool)
eorEnabled = lens _eorEnabled (\s a -> s {_eorEnabled = a})

instance Hashable EnclaveOptionsRequest

instance NFData EnclaveOptionsRequest

instance ToQuery EnclaveOptionsRequest where
  toQuery EnclaveOptionsRequest' {..} =
    mconcat ["Enabled" =: _eorEnabled]
