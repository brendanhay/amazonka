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
-- Module      : Network.AWS.IAM.UpdateServerCertificate
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified server certificate.
--
-- For more information about working with server certificates, including a
-- list of AWS services that can use the server certificates that you
-- manage with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates>
-- in the /IAM User Guide/.
--
-- You should understand the implications of changing a server
-- certificate\'s path or name. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts Renaming a Server Certificate>
-- in the /IAM User Guide/.
--
-- To change a server certificate name the requester must have appropriate
-- permissions on both the source object and the target object. For
-- example, to change the name from ProductionCert to ProdCert, the entity
-- making the request must have permission on ProductionCert and ProdCert,
-- or must have permission on all (*). For more information about
-- permissions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UpdateServerCertificate
    (
    -- * Creating a Request
      updateServerCertificate
    , UpdateServerCertificate
    -- * Request Lenses
    , uNewServerCertificateName
    , uNewPath
    , uServerCertificateName

    -- * Destructuring the Response
    , updateServerCertificateResponse
    , UpdateServerCertificateResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateServerCertificate' smart constructor.
data UpdateServerCertificate = UpdateServerCertificate'
    { _uNewServerCertificateName :: !(Maybe Text)
    , _uNewPath                  :: !(Maybe Text)
    , _uServerCertificateName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uNewServerCertificateName'
--
-- * 'uNewPath'
--
-- * 'uServerCertificateName'
updateServerCertificate
    :: Text -- ^ 'uServerCertificateName'
    -> UpdateServerCertificate
updateServerCertificate pServerCertificateName_ =
    UpdateServerCertificate'
    { _uNewServerCertificateName = Nothing
    , _uNewPath = Nothing
    , _uServerCertificateName = pServerCertificateName_
    }

-- | The new name for the server certificate. Include this only if you are
-- updating the server certificate\'s name. The name of the certificate
-- cannot contain any spaces.
uNewServerCertificateName :: Lens' UpdateServerCertificate (Maybe Text)
uNewServerCertificateName = lens _uNewServerCertificateName (\ s a -> s{_uNewServerCertificateName = a});

-- | The new path for the server certificate. Include this only if you are
-- updating the server certificate\'s path.
uNewPath :: Lens' UpdateServerCertificate (Maybe Text)
uNewPath = lens _uNewPath (\ s a -> s{_uNewPath = a});

-- | The name of the server certificate that you want to update.
uServerCertificateName :: Lens' UpdateServerCertificate Text
uServerCertificateName = lens _uServerCertificateName (\ s a -> s{_uServerCertificateName = a});

instance AWSRequest UpdateServerCertificate where
        type Rs UpdateServerCertificate =
             UpdateServerCertificateResponse
        request = postQuery iam
        response
          = receiveNull UpdateServerCertificateResponse'

instance Hashable UpdateServerCertificate

instance ToHeaders UpdateServerCertificate where
        toHeaders = const mempty

instance ToPath UpdateServerCertificate where
        toPath = const "/"

instance ToQuery UpdateServerCertificate where
        toQuery UpdateServerCertificate'{..}
          = mconcat
              ["Action" =:
                 ("UpdateServerCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "NewServerCertificateName" =:
                 _uNewServerCertificateName,
               "NewPath" =: _uNewPath,
               "ServerCertificateName" =: _uServerCertificateName]

-- | /See:/ 'updateServerCertificateResponse' smart constructor.
data UpdateServerCertificateResponse =
    UpdateServerCertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateServerCertificateResponse' with the minimum fields required to make a request.
--
updateServerCertificateResponse
    :: UpdateServerCertificateResponse
updateServerCertificateResponse = UpdateServerCertificateResponse'
