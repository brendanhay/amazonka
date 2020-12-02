{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ResourceIdentifier where

import Network.AWS.IoT.Types.PolicyVersionIdentifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information that identifies the noncompliant resource.
--
--
--
-- /See:/ 'resourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { _riIamRoleARN ::
      !(Maybe Text),
    _riClientId :: !(Maybe Text),
    _riRoleAliasARN :: !(Maybe Text),
    _riCaCertificateId :: !(Maybe Text),
    _riDeviceCertificateId :: !(Maybe Text),
    _riAccount :: !(Maybe Text),
    _riPolicyVersionIdentifier ::
      !(Maybe PolicyVersionIdentifier),
    _riCognitoIdentityPoolId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riIamRoleARN' - The ARN of the IAM role that has overly permissive actions.
--
-- * 'riClientId' - The client ID.
--
-- * 'riRoleAliasARN' - The ARN of the role alias that has overly permissive actions.
--
-- * 'riCaCertificateId' - The ID of the CA certificate used to authorize the certificate.
--
-- * 'riDeviceCertificateId' - The ID of the certificate attached to the resource.
--
-- * 'riAccount' - The account with which the resource is associated.
--
-- * 'riPolicyVersionIdentifier' - The version of the policy associated with the resource.
--
-- * 'riCognitoIdentityPoolId' - The ID of the Amazon Cognito identity pool.
resourceIdentifier ::
  ResourceIdentifier
resourceIdentifier =
  ResourceIdentifier'
    { _riIamRoleARN = Nothing,
      _riClientId = Nothing,
      _riRoleAliasARN = Nothing,
      _riCaCertificateId = Nothing,
      _riDeviceCertificateId = Nothing,
      _riAccount = Nothing,
      _riPolicyVersionIdentifier = Nothing,
      _riCognitoIdentityPoolId = Nothing
    }

-- | The ARN of the IAM role that has overly permissive actions.
riIamRoleARN :: Lens' ResourceIdentifier (Maybe Text)
riIamRoleARN = lens _riIamRoleARN (\s a -> s {_riIamRoleARN = a})

-- | The client ID.
riClientId :: Lens' ResourceIdentifier (Maybe Text)
riClientId = lens _riClientId (\s a -> s {_riClientId = a})

-- | The ARN of the role alias that has overly permissive actions.
riRoleAliasARN :: Lens' ResourceIdentifier (Maybe Text)
riRoleAliasARN = lens _riRoleAliasARN (\s a -> s {_riRoleAliasARN = a})

-- | The ID of the CA certificate used to authorize the certificate.
riCaCertificateId :: Lens' ResourceIdentifier (Maybe Text)
riCaCertificateId = lens _riCaCertificateId (\s a -> s {_riCaCertificateId = a})

-- | The ID of the certificate attached to the resource.
riDeviceCertificateId :: Lens' ResourceIdentifier (Maybe Text)
riDeviceCertificateId = lens _riDeviceCertificateId (\s a -> s {_riDeviceCertificateId = a})

-- | The account with which the resource is associated.
riAccount :: Lens' ResourceIdentifier (Maybe Text)
riAccount = lens _riAccount (\s a -> s {_riAccount = a})

-- | The version of the policy associated with the resource.
riPolicyVersionIdentifier :: Lens' ResourceIdentifier (Maybe PolicyVersionIdentifier)
riPolicyVersionIdentifier = lens _riPolicyVersionIdentifier (\s a -> s {_riPolicyVersionIdentifier = a})

-- | The ID of the Amazon Cognito identity pool.
riCognitoIdentityPoolId :: Lens' ResourceIdentifier (Maybe Text)
riCognitoIdentityPoolId = lens _riCognitoIdentityPoolId (\s a -> s {_riCognitoIdentityPoolId = a})

instance FromJSON ResourceIdentifier where
  parseJSON =
    withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            <$> (x .:? "iamRoleArn")
            <*> (x .:? "clientId")
            <*> (x .:? "roleAliasArn")
            <*> (x .:? "caCertificateId")
            <*> (x .:? "deviceCertificateId")
            <*> (x .:? "account")
            <*> (x .:? "policyVersionIdentifier")
            <*> (x .:? "cognitoIdentityPoolId")
      )

instance Hashable ResourceIdentifier

instance NFData ResourceIdentifier

instance ToJSON ResourceIdentifier where
  toJSON ResourceIdentifier' {..} =
    object
      ( catMaybes
          [ ("iamRoleArn" .=) <$> _riIamRoleARN,
            ("clientId" .=) <$> _riClientId,
            ("roleAliasArn" .=) <$> _riRoleAliasARN,
            ("caCertificateId" .=) <$> _riCaCertificateId,
            ("deviceCertificateId" .=) <$> _riDeviceCertificateId,
            ("account" .=) <$> _riAccount,
            ("policyVersionIdentifier" .=) <$> _riPolicyVersionIdentifier,
            ("cognitoIdentityPoolId" .=) <$> _riCognitoIdentityPoolId
          ]
      )
