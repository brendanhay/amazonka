{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateEnclaveCertificateIAMRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM role from an AWS Certificate Manager (ACM) certificate. Disassociating an IAM role from an ACM certificate removes the Amazon S3 object that contains the certificate, certificate chain, and encrypted private key from the Amazon S3 bucket. It also revokes the IAM role's permission to use the AWS Key Management Service (KMS) customer master key (CMK) used to encrypt the private key. This effectively revokes the role's permission to use the certificate.
module Network.AWS.EC2.DisassociateEnclaveCertificateIAMRole
  ( -- * Creating a Request
    disassociateEnclaveCertificateIAMRole,
    DisassociateEnclaveCertificateIAMRole,

    -- * Request Lenses
    decirCertificateARN,
    decirDryRun,
    decirRoleARN,

    -- * Destructuring the Response
    disassociateEnclaveCertificateIAMRoleResponse,
    DisassociateEnclaveCertificateIAMRoleResponse,

    -- * Response Lenses
    decirrsReturn,
    decirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateEnclaveCertificateIAMRole' smart constructor.
data DisassociateEnclaveCertificateIAMRole = DisassociateEnclaveCertificateIAMRole'
  { _decirCertificateARN ::
      !(Maybe Text),
    _decirDryRun ::
      !(Maybe Bool),
    _decirRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateEnclaveCertificateIAMRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decirCertificateARN' - The ARN of the ACM certificate from which to disassociate the IAM role.
--
-- * 'decirDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'decirRoleARN' - The ARN of the IAM role to disassociate.
disassociateEnclaveCertificateIAMRole ::
  DisassociateEnclaveCertificateIAMRole
disassociateEnclaveCertificateIAMRole =
  DisassociateEnclaveCertificateIAMRole'
    { _decirCertificateARN =
        Nothing,
      _decirDryRun = Nothing,
      _decirRoleARN = Nothing
    }

-- | The ARN of the ACM certificate from which to disassociate the IAM role.
decirCertificateARN :: Lens' DisassociateEnclaveCertificateIAMRole (Maybe Text)
decirCertificateARN = lens _decirCertificateARN (\s a -> s {_decirCertificateARN = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
decirDryRun :: Lens' DisassociateEnclaveCertificateIAMRole (Maybe Bool)
decirDryRun = lens _decirDryRun (\s a -> s {_decirDryRun = a})

-- | The ARN of the IAM role to disassociate.
decirRoleARN :: Lens' DisassociateEnclaveCertificateIAMRole (Maybe Text)
decirRoleARN = lens _decirRoleARN (\s a -> s {_decirRoleARN = a})

instance AWSRequest DisassociateEnclaveCertificateIAMRole where
  type
    Rs DisassociateEnclaveCertificateIAMRole =
      DisassociateEnclaveCertificateIAMRoleResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DisassociateEnclaveCertificateIAMRoleResponse'
            <$> (x .@? "return") <*> (pure (fromEnum s))
      )

instance Hashable DisassociateEnclaveCertificateIAMRole

instance NFData DisassociateEnclaveCertificateIAMRole

instance ToHeaders DisassociateEnclaveCertificateIAMRole where
  toHeaders = const mempty

instance ToPath DisassociateEnclaveCertificateIAMRole where
  toPath = const "/"

instance ToQuery DisassociateEnclaveCertificateIAMRole where
  toQuery DisassociateEnclaveCertificateIAMRole' {..} =
    mconcat
      [ "Action"
          =: ("DisassociateEnclaveCertificateIamRole" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "CertificateArn" =: _decirCertificateARN,
        "DryRun" =: _decirDryRun,
        "RoleArn" =: _decirRoleARN
      ]

-- | /See:/ 'disassociateEnclaveCertificateIAMRoleResponse' smart constructor.
data DisassociateEnclaveCertificateIAMRoleResponse = DisassociateEnclaveCertificateIAMRoleResponse'
  { _decirrsReturn ::
      !( Maybe
           Bool
       ),
    _decirrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisassociateEnclaveCertificateIAMRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decirrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'decirrsResponseStatus' - -- | The response status code.
disassociateEnclaveCertificateIAMRoleResponse ::
  -- | 'decirrsResponseStatus'
  Int ->
  DisassociateEnclaveCertificateIAMRoleResponse
disassociateEnclaveCertificateIAMRoleResponse pResponseStatus_ =
  DisassociateEnclaveCertificateIAMRoleResponse'
    { _decirrsReturn =
        Nothing,
      _decirrsResponseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
decirrsReturn :: Lens' DisassociateEnclaveCertificateIAMRoleResponse (Maybe Bool)
decirrsReturn = lens _decirrsReturn (\s a -> s {_decirrsReturn = a})

-- | -- | The response status code.
decirrsResponseStatus :: Lens' DisassociateEnclaveCertificateIAMRoleResponse Int
decirrsResponseStatus = lens _decirrsResponseStatus (\s a -> s {_decirrsResponseStatus = a})

instance NFData DisassociateEnclaveCertificateIAMRoleResponse
