{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.CreatePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns permissions from a private CA to a designated AWS service. Services are specified by their service principals and can be given permission to create and retrieve certificates on a private CA. Services can also be given permission to list the active permissions that the private CA has granted. For ACM to automatically renew your private CA's certificates, you must assign all possible permissions from the CA to the ACM service principal.
--
--
-- At this time, you can only assign permissions to ACM (@acm.amazonaws.com@ ). Permissions can be revoked with the 'DeletePermission' operation and listed with the 'ListPermissions' operation.
--
module Network.AWS.CertificateManagerPCA.CreatePermission
    (
    -- * Creating a Request
      createPermission
    , CreatePermission
    -- * Request Lenses
    , cpSourceAccount
    , cpCertificateAuthorityARN
    , cpPrincipal
    , cpActions

    -- * Destructuring the Response
    , createPermissionResponse
    , CreatePermissionResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPermission' smart constructor.
data CreatePermission = CreatePermission'
  { _cpSourceAccount           :: !(Maybe Text)
  , _cpCertificateAuthorityARN :: !Text
  , _cpPrincipal               :: !Text
  , _cpActions                 :: !(List1 ActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpSourceAccount' - The ID of the calling account.
--
-- * 'cpCertificateAuthorityARN' - The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the 'ListCertificateAuthorities' operation. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'cpPrincipal' - The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
--
-- * 'cpActions' - The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
createPermission
    :: Text -- ^ 'cpCertificateAuthorityARN'
    -> Text -- ^ 'cpPrincipal'
    -> NonEmpty ActionType -- ^ 'cpActions'
    -> CreatePermission
createPermission pCertificateAuthorityARN_ pPrincipal_ pActions_ =
  CreatePermission'
    { _cpSourceAccount = Nothing
    , _cpCertificateAuthorityARN = pCertificateAuthorityARN_
    , _cpPrincipal = pPrincipal_
    , _cpActions = _List1 # pActions_
    }


-- | The ID of the calling account.
cpSourceAccount :: Lens' CreatePermission (Maybe Text)
cpSourceAccount = lens _cpSourceAccount (\ s a -> s{_cpSourceAccount = a})

-- | The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the 'ListCertificateAuthorities' operation. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
cpCertificateAuthorityARN :: Lens' CreatePermission Text
cpCertificateAuthorityARN = lens _cpCertificateAuthorityARN (\ s a -> s{_cpCertificateAuthorityARN = a})

-- | The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
cpPrincipal :: Lens' CreatePermission Text
cpPrincipal = lens _cpPrincipal (\ s a -> s{_cpPrincipal = a})

-- | The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
cpActions :: Lens' CreatePermission (NonEmpty ActionType)
cpActions = lens _cpActions (\ s a -> s{_cpActions = a}) . _List1

instance AWSRequest CreatePermission where
        type Rs CreatePermission = CreatePermissionResponse
        request = postJSON certificateManagerPCA
        response = receiveNull CreatePermissionResponse'

instance Hashable CreatePermission where

instance NFData CreatePermission where

instance ToHeaders CreatePermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.CreatePermission" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePermission where
        toJSON CreatePermission'{..}
          = object
              (catMaybes
                 [("SourceAccount" .=) <$> _cpSourceAccount,
                  Just
                    ("CertificateAuthorityArn" .=
                       _cpCertificateAuthorityARN),
                  Just ("Principal" .= _cpPrincipal),
                  Just ("Actions" .= _cpActions)])

instance ToPath CreatePermission where
        toPath = const "/"

instance ToQuery CreatePermission where
        toQuery = const mempty

-- | /See:/ 'createPermissionResponse' smart constructor.
data CreatePermissionResponse =
  CreatePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePermissionResponse' with the minimum fields required to make a request.
--
createPermissionResponse
    :: CreatePermissionResponse
createPermissionResponse = CreatePermissionResponse'


instance NFData CreatePermissionResponse where
