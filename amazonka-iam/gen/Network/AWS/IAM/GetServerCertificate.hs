{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified server certificate.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificate.html>
module Network.AWS.IAM.GetServerCertificate
    (
    -- * Request
      GetServerCertificate
    -- ** Request constructor
    , getServerCertificate
    -- ** Request lenses
    , gscrqServerCertificateName

    -- * Response
    , GetServerCertificateResponse
    -- ** Response constructor
    , getServerCertificateResponse
    -- ** Response lenses
    , gscrsStatus
    , gscrsServerCertificate
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gscrqServerCertificateName'
newtype GetServerCertificate = GetServerCertificate'
    { _gscrqServerCertificateName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetServerCertificate' smart constructor.
getServerCertificate :: Text -> GetServerCertificate
getServerCertificate pServerCertificateName =
    GetServerCertificate'
    { _gscrqServerCertificateName = pServerCertificateName
    }

-- | The name of the server certificate you want to retrieve information
-- about.
gscrqServerCertificateName :: Lens' GetServerCertificate Text
gscrqServerCertificateName = lens _gscrqServerCertificateName (\ s a -> s{_gscrqServerCertificateName = a});

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
               "ServerCertificateName" =:
                 _gscrqServerCertificateName]

-- | Contains the response to a successful GetServerCertificate request.
--
-- /See:/ 'getServerCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gscrsStatus'
--
-- * 'gscrsServerCertificate'
data GetServerCertificateResponse = GetServerCertificateResponse'
    { _gscrsStatus            :: !Int
    , _gscrsServerCertificate :: !ServerCertificate
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetServerCertificateResponse' smart constructor.
getServerCertificateResponse :: Int -> ServerCertificate -> GetServerCertificateResponse
getServerCertificateResponse pStatus pServerCertificate =
    GetServerCertificateResponse'
    { _gscrsStatus = pStatus
    , _gscrsServerCertificate = pServerCertificate
    }

-- | FIXME: Undocumented member.
gscrsStatus :: Lens' GetServerCertificateResponse Int
gscrsStatus = lens _gscrsStatus (\ s a -> s{_gscrsStatus = a});

-- | Information about the server certificate.
gscrsServerCertificate :: Lens' GetServerCertificateResponse ServerCertificate
gscrsServerCertificate = lens _gscrsServerCertificate (\ s a -> s{_gscrsServerCertificate = a});
