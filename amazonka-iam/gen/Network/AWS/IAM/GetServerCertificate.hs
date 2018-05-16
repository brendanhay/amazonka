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
-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified server certificate stored in IAM.
--
--
-- For more information about working with server certificates, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic includes a list of AWS services that can use the server certificates that you manage with IAM.
--
module Network.AWS.IAM.GetServerCertificate
    (
    -- * Creating a Request
      getServerCertificate
    , GetServerCertificate
    -- * Request Lenses
    , gscServerCertificateName

    -- * Destructuring the Response
    , getServerCertificateResponse
    , GetServerCertificateResponse
    -- * Response Lenses
    , gscrsResponseStatus
    , gscrsServerCertificate
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getServerCertificate' smart constructor.
newtype GetServerCertificate = GetServerCertificate'
  { _gscServerCertificateName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscServerCertificateName' - The name of the server certificate you want to retrieve information about. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
getServerCertificate
    :: Text -- ^ 'gscServerCertificateName'
    -> GetServerCertificate
getServerCertificate pServerCertificateName_ =
  GetServerCertificate' {_gscServerCertificateName = pServerCertificateName_}


-- | The name of the server certificate you want to retrieve information about. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
gscServerCertificateName :: Lens' GetServerCertificate Text
gscServerCertificateName = lens _gscServerCertificateName (\ s a -> s{_gscServerCertificateName = a})

instance AWSRequest GetServerCertificate where
        type Rs GetServerCertificate =
             GetServerCertificateResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetServerCertificateResult"
              (\ s h x ->
                 GetServerCertificateResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ServerCertificate"))

instance Hashable GetServerCertificate where

instance NFData GetServerCertificate where

instance ToHeaders GetServerCertificate where
        toHeaders = const mempty

instance ToPath GetServerCertificate where
        toPath = const "/"

instance ToQuery GetServerCertificate where
        toQuery GetServerCertificate'{..}
          = mconcat
              ["Action" =: ("GetServerCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ServerCertificateName" =: _gscServerCertificateName]

-- | Contains the response to a successful 'GetServerCertificate' request.
--
--
--
-- /See:/ 'getServerCertificateResponse' smart constructor.
data GetServerCertificateResponse = GetServerCertificateResponse'
  { _gscrsResponseStatus    :: !Int
  , _gscrsServerCertificate :: !ServerCertificate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServerCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscrsResponseStatus' - -- | The response status code.
--
-- * 'gscrsServerCertificate' - A structure containing details about the server certificate.
getServerCertificateResponse
    :: Int -- ^ 'gscrsResponseStatus'
    -> ServerCertificate -- ^ 'gscrsServerCertificate'
    -> GetServerCertificateResponse
getServerCertificateResponse pResponseStatus_ pServerCertificate_ =
  GetServerCertificateResponse'
    { _gscrsResponseStatus = pResponseStatus_
    , _gscrsServerCertificate = pServerCertificate_
    }


-- | -- | The response status code.
gscrsResponseStatus :: Lens' GetServerCertificateResponse Int
gscrsResponseStatus = lens _gscrsResponseStatus (\ s a -> s{_gscrsResponseStatus = a})

-- | A structure containing details about the server certificate.
gscrsServerCertificate :: Lens' GetServerCertificateResponse ServerCertificate
gscrsServerCertificate = lens _gscrsServerCertificate (\ s a -> s{_gscrsServerCertificate = a})

instance NFData GetServerCertificateResponse where
