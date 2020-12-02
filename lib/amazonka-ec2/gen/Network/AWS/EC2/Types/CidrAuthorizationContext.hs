{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CidrAuthorizationContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CidrAuthorizationContext where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides authorization for Amazon to bring a specific IP address range to a specific AWS account using bring your own IP addresses (BYOIP). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html#prepare-for-byoip Prepare to Bring Your Address Range to Your AWS Account> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'cidrAuthorizationContext' smart constructor.
data CidrAuthorizationContext = CidrAuthorizationContext'
  { _cacMessage ::
      !Text,
    _cacSignature :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CidrAuthorizationContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacMessage' - The plain-text authorization message for the prefix and account.
--
-- * 'cacSignature' - The signed authorization message for the prefix and account.
cidrAuthorizationContext ::
  -- | 'cacMessage'
  Text ->
  -- | 'cacSignature'
  Text ->
  CidrAuthorizationContext
cidrAuthorizationContext pMessage_ pSignature_ =
  CidrAuthorizationContext'
    { _cacMessage = pMessage_,
      _cacSignature = pSignature_
    }

-- | The plain-text authorization message for the prefix and account.
cacMessage :: Lens' CidrAuthorizationContext Text
cacMessage = lens _cacMessage (\s a -> s {_cacMessage = a})

-- | The signed authorization message for the prefix and account.
cacSignature :: Lens' CidrAuthorizationContext Text
cacSignature = lens _cacSignature (\s a -> s {_cacSignature = a})

instance Hashable CidrAuthorizationContext

instance NFData CidrAuthorizationContext

instance ToQuery CidrAuthorizationContext where
  toQuery CidrAuthorizationContext' {..} =
    mconcat ["Message" =: _cacMessage, "Signature" =: _cacSignature]
