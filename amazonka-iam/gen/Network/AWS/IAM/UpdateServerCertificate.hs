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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified server certificate stored in IAM.
--
--
-- For more information about working with server certificates, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
--
-- /Important:/ You should understand the implications of changing a server certificate's path or name. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts Renaming a Server Certificate> in the /IAM User Guide/ .
--
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

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateServerCertificate' smart constructor.
data UpdateServerCertificate = UpdateServerCertificate'
  { _uNewServerCertificateName :: !(Maybe Text)
  , _uNewPath                  :: !(Maybe Text)
  , _uServerCertificateName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uNewServerCertificateName' - The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'uNewPath' - The new path for the server certificate. Include this only if you are updating the server certificate's path. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'uServerCertificateName' - The name of the server certificate that you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
updateServerCertificate
    :: Text -- ^ 'uServerCertificateName'
    -> UpdateServerCertificate
updateServerCertificate pServerCertificateName_ =
  UpdateServerCertificate'
    { _uNewServerCertificateName = Nothing
    , _uNewPath = Nothing
    , _uServerCertificateName = pServerCertificateName_
    }


-- | The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uNewServerCertificateName :: Lens' UpdateServerCertificate (Maybe Text)
uNewServerCertificateName = lens _uNewServerCertificateName (\ s a -> s{_uNewServerCertificateName = a})

-- | The new path for the server certificate. Include this only if you are updating the server certificate's path. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
uNewPath :: Lens' UpdateServerCertificate (Maybe Text)
uNewPath = lens _uNewPath (\ s a -> s{_uNewPath = a})

-- | The name of the server certificate that you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uServerCertificateName :: Lens' UpdateServerCertificate Text
uServerCertificateName = lens _uServerCertificateName (\ s a -> s{_uServerCertificateName = a})

instance AWSRequest UpdateServerCertificate where
        type Rs UpdateServerCertificate =
             UpdateServerCertificateResponse
        request = postQuery iam
        response
          = receiveNull UpdateServerCertificateResponse'

instance Hashable UpdateServerCertificate where

instance NFData UpdateServerCertificate where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServerCertificateResponse' with the minimum fields required to make a request.
--
updateServerCertificateResponse
    :: UpdateServerCertificateResponse
updateServerCertificateResponse = UpdateServerCertificateResponse'


instance NFData UpdateServerCertificateResponse where
