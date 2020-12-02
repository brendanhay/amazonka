{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.FederatedUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types.FederatedUser where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifiers for the federated user that is associated with the credentials.
--
--
--
-- /See:/ 'federatedUser' smart constructor.
data FederatedUser = FederatedUser'
  { _fuFederatedUserId :: !Text,
    _fuARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FederatedUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fuFederatedUserId' - The string that identifies the federated user associated with the credentials, similar to the unique ID of an IAM user.
--
-- * 'fuARN' - The ARN that specifies the federated user that is associated with the credentials. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
federatedUser ::
  -- | 'fuFederatedUserId'
  Text ->
  -- | 'fuARN'
  Text ->
  FederatedUser
federatedUser pFederatedUserId_ pARN_ =
  FederatedUser'
    { _fuFederatedUserId = pFederatedUserId_,
      _fuARN = pARN_
    }

-- | The string that identifies the federated user associated with the credentials, similar to the unique ID of an IAM user.
fuFederatedUserId :: Lens' FederatedUser Text
fuFederatedUserId = lens _fuFederatedUserId (\s a -> s {_fuFederatedUserId = a})

-- | The ARN that specifies the federated user that is associated with the credentials. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
fuARN :: Lens' FederatedUser Text
fuARN = lens _fuARN (\s a -> s {_fuARN = a})

instance FromXML FederatedUser where
  parseXML x =
    FederatedUser' <$> (x .@ "FederatedUserId") <*> (x .@ "Arn")

instance Hashable FederatedUser

instance NFData FederatedUser
