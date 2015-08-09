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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified server certificate.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificate.html AWS API Reference> for GetServerCertificate.
module Network.AWS.IAM.GetServerCertificate
    (
    -- * Creating a Request
      GetServerCertificate
    , getServerCertificate
    -- * Request Lenses
    , gscServerCertificateName

    -- * Destructuring the Response
    , GetServerCertificateResponse
    , getServerCertificateResponse
    -- * Response Lenses
    , gscrsStatus
    , gscrsServerCertificate
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetServerCertificate' smart constructor.
getServerCertificate :: Text -> GetServerCertificate
getServerCertificate pServerCertificateName_ =
    GetServerCertificate'
    { _gscServerCertificateName = pServerCertificateName_
    }

-- | The name of the server certificate you want to retrieve information
-- about.
gscServerCertificateName :: Lens' GetServerCertificate Text
gscServerCertificateName = lens _gscServerCertificateName (\ s a -> s{_gscServerCertificateName = a});

instance AWSRequest GetServerCertificate where
        type Sv GetServerCertificate = IAM
        type Rs GetServerCertificate =
             GetServerCertificateResponse
        request = postQuery
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
-- * 'gscrsStatus'
--
-- * 'gscrsServerCertificate'
data GetServerCertificateResponse = GetServerCertificateResponse'
    { _gscrsStatus            :: !Int
    , _gscrsServerCertificate :: !ServerCertificate
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetServerCertificateResponse' smart constructor.
getServerCertificateResponse :: Int -> ServerCertificate -> GetServerCertificateResponse
getServerCertificateResponse pStatus_ pServerCertificate_ =
    GetServerCertificateResponse'
    { _gscrsStatus = pStatus_
    , _gscrsServerCertificate = pServerCertificate_
    }

-- | Undocumented member.
gscrsStatus :: Lens' GetServerCertificateResponse Int
gscrsStatus = lens _gscrsStatus (\ s a -> s{_gscrsStatus = a});

-- | Information about the server certificate.
gscrsServerCertificate :: Lens' GetServerCertificateResponse ServerCertificate
gscrsServerCertificate = lens _gscrsServerCertificate (\ s a -> s{_gscrsServerCertificate = a});
