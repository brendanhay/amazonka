{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about the specified server certificate.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificate.html>
module Network.AWS.IAM.GetServerCertificate
    (
    -- * Request
      GetServerCertificate
    -- ** Request constructor
    , getServerCertificate
    -- ** Request lenses
    , gscServerCertificateName

    -- * Response
    , GetServerCertificateResponse
    -- ** Response constructor
    , getServerCertificateResponse
    -- ** Response lenses
    , gscrStatus
    , gscrServerCertificate
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gscServerCertificateName'
newtype GetServerCertificate = GetServerCertificate'
    { _gscServerCertificateName :: Text
    } deriving (Eq,Read,Show)

-- | 'GetServerCertificate' smart constructor.
getServerCertificate :: Text -> GetServerCertificate
getServerCertificate pServerCertificateName =
    GetServerCertificate'
    { _gscServerCertificateName = pServerCertificateName
    }

-- | The name of the server certificate you want to retrieve information
-- about.
gscServerCertificateName :: Lens' GetServerCertificate Text
gscServerCertificateName = lens _gscServerCertificateName (\ s a -> s{_gscServerCertificateName = a});

instance AWSRequest GetServerCertificate where
        type Sv GetServerCertificate = IAM
        type Rs GetServerCertificate =
             GetServerCertificateResponse
        request = post
        response
          = receiveXMLWrapper "GetServerCertificateResult"
              (\ s h x ->
                 GetServerCertificateResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ServerCertificate"))

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

-- | Contains the response to a successful GetServerCertificate request.
--
-- /See:/ 'getServerCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gscrStatus'
--
-- * 'gscrServerCertificate'
data GetServerCertificateResponse = GetServerCertificateResponse'
    { _gscrStatus            :: !Int
    , _gscrServerCertificate :: !ServerCertificate
    } deriving (Eq,Read,Show)

-- | 'GetServerCertificateResponse' smart constructor.
getServerCertificateResponse :: Int -> ServerCertificate -> GetServerCertificateResponse
getServerCertificateResponse pStatus pServerCertificate =
    GetServerCertificateResponse'
    { _gscrStatus = pStatus
    , _gscrServerCertificate = pServerCertificate
    }

-- | FIXME: Undocumented member.
gscrStatus :: Lens' GetServerCertificateResponse Int
gscrStatus = lens _gscrStatus (\ s a -> s{_gscrStatus = a});

-- | Information about the server certificate.
gscrServerCertificate :: Lens' GetServerCertificateResponse ServerCertificate
gscrServerCertificate = lens _gscrServerCertificate (\ s a -> s{_gscrServerCertificate = a});
