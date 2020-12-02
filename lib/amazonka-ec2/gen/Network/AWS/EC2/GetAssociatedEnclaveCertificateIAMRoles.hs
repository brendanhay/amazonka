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
-- Module      : Network.AWS.EC2.GetAssociatedEnclaveCertificateIAMRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM roles that are associated with the specified AWS Certificate Manager (ACM) certificate. It also returns the name of the Amazon S3 bucket and the Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored, and the ARN of the AWS Key Management Service (KMS) customer master key (CMK) that's used to encrypt the private key.
module Network.AWS.EC2.GetAssociatedEnclaveCertificateIAMRoles
  ( -- * Creating a Request
    getAssociatedEnclaveCertificateIAMRoles,
    GetAssociatedEnclaveCertificateIAMRoles,

    -- * Request Lenses
    gaecirCertificateARN,
    gaecirDryRun,

    -- * Destructuring the Response
    getAssociatedEnclaveCertificateIAMRolesResponse,
    GetAssociatedEnclaveCertificateIAMRolesResponse,

    -- * Response Lenses
    gaecirrsAssociatedRoles,
    gaecirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAssociatedEnclaveCertificateIAMRoles' smart constructor.
data GetAssociatedEnclaveCertificateIAMRoles = GetAssociatedEnclaveCertificateIAMRoles'
  { _gaecirCertificateARN ::
      !( Maybe
           Text
       ),
    _gaecirDryRun ::
      !( Maybe
           Bool
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAssociatedEnclaveCertificateIAMRoles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaecirCertificateARN' - The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
--
-- * 'gaecirDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
getAssociatedEnclaveCertificateIAMRoles ::
  GetAssociatedEnclaveCertificateIAMRoles
getAssociatedEnclaveCertificateIAMRoles =
  GetAssociatedEnclaveCertificateIAMRoles'
    { _gaecirCertificateARN =
        Nothing,
      _gaecirDryRun = Nothing
    }

-- | The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
gaecirCertificateARN :: Lens' GetAssociatedEnclaveCertificateIAMRoles (Maybe Text)
gaecirCertificateARN = lens _gaecirCertificateARN (\s a -> s {_gaecirCertificateARN = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gaecirDryRun :: Lens' GetAssociatedEnclaveCertificateIAMRoles (Maybe Bool)
gaecirDryRun = lens _gaecirDryRun (\s a -> s {_gaecirDryRun = a})

instance AWSRequest GetAssociatedEnclaveCertificateIAMRoles where
  type
    Rs GetAssociatedEnclaveCertificateIAMRoles =
      GetAssociatedEnclaveCertificateIAMRolesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetAssociatedEnclaveCertificateIAMRolesResponse'
            <$> ( x .@? "associatedRoleSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable GetAssociatedEnclaveCertificateIAMRoles

instance NFData GetAssociatedEnclaveCertificateIAMRoles

instance ToHeaders GetAssociatedEnclaveCertificateIAMRoles where
  toHeaders = const mempty

instance ToPath GetAssociatedEnclaveCertificateIAMRoles where
  toPath = const "/"

instance ToQuery GetAssociatedEnclaveCertificateIAMRoles where
  toQuery GetAssociatedEnclaveCertificateIAMRoles' {..} =
    mconcat
      [ "Action"
          =: ("GetAssociatedEnclaveCertificateIamRoles" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "CertificateArn" =: _gaecirCertificateARN,
        "DryRun" =: _gaecirDryRun
      ]

-- | /See:/ 'getAssociatedEnclaveCertificateIAMRolesResponse' smart constructor.
data GetAssociatedEnclaveCertificateIAMRolesResponse = GetAssociatedEnclaveCertificateIAMRolesResponse'
  { _gaecirrsAssociatedRoles ::
      !( Maybe
           [AssociatedRole]
       ),
    _gaecirrsResponseStatus ::
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

-- | Creates a value of 'GetAssociatedEnclaveCertificateIAMRolesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaecirrsAssociatedRoles' - Information about the associated IAM roles.
--
-- * 'gaecirrsResponseStatus' - -- | The response status code.
getAssociatedEnclaveCertificateIAMRolesResponse ::
  -- | 'gaecirrsResponseStatus'
  Int ->
  GetAssociatedEnclaveCertificateIAMRolesResponse
getAssociatedEnclaveCertificateIAMRolesResponse pResponseStatus_ =
  GetAssociatedEnclaveCertificateIAMRolesResponse'
    { _gaecirrsAssociatedRoles =
        Nothing,
      _gaecirrsResponseStatus = pResponseStatus_
    }

-- | Information about the associated IAM roles.
gaecirrsAssociatedRoles :: Lens' GetAssociatedEnclaveCertificateIAMRolesResponse [AssociatedRole]
gaecirrsAssociatedRoles = lens _gaecirrsAssociatedRoles (\s a -> s {_gaecirrsAssociatedRoles = a}) . _Default . _Coerce

-- | -- | The response status code.
gaecirrsResponseStatus :: Lens' GetAssociatedEnclaveCertificateIAMRolesResponse Int
gaecirrsResponseStatus = lens _gaecirrsResponseStatus (\s a -> s {_gaecirrsResponseStatus = a})

instance NFData GetAssociatedEnclaveCertificateIAMRolesResponse
