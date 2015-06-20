{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.UpdateServerCertificate
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

-- | Updates the name and\/or the path of the specified server certificate.
--
-- You should understand the implications of changing a server
-- certificate\'s path or name. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingServerCerts.html Managing Server Certificates>
-- in the /Using IAM/ guide.
--
-- To change a server certificate name the requester must have appropriate
-- permissions on both the source object and the target object. For
-- example, to change the name from ProductionCert to ProdCert, the entity
-- making the request must have permission on ProductionCert and ProdCert,
-- or must have permission on all (*). For more information about
-- permissions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html Permissions and Policies>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateServerCertificate.html>
module Network.AWS.IAM.UpdateServerCertificate
    (
    -- * Request
      UpdateServerCertificate
    -- ** Request constructor
    , updateServerCertificate
    -- ** Request lenses
    , updNewServerCertificateName
    , updNewPath
    , updServerCertificateName

    -- * Response
    , UpdateServerCertificateResponse
    -- ** Response constructor
    , updateServerCertificateResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'updNewServerCertificateName'
--
-- * 'updNewPath'
--
-- * 'updServerCertificateName'
data UpdateServerCertificate = UpdateServerCertificate'{_updNewServerCertificateName :: Maybe Text, _updNewPath :: Maybe Text, _updServerCertificateName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateServerCertificate' smart constructor.
updateServerCertificate :: Text -> UpdateServerCertificate
updateServerCertificate pServerCertificateName = UpdateServerCertificate'{_updNewServerCertificateName = Nothing, _updNewPath = Nothing, _updServerCertificateName = pServerCertificateName};

-- | The new name for the server certificate. Include this only if you are
-- updating the server certificate\'s name. The name of the certificate
-- cannot contain any spaces.
updNewServerCertificateName :: Lens' UpdateServerCertificate (Maybe Text)
updNewServerCertificateName = lens _updNewServerCertificateName (\ s a -> s{_updNewServerCertificateName = a});

-- | The new path for the server certificate. Include this only if you are
-- updating the server certificate\'s path.
updNewPath :: Lens' UpdateServerCertificate (Maybe Text)
updNewPath = lens _updNewPath (\ s a -> s{_updNewPath = a});

-- | The name of the server certificate that you want to update.
updServerCertificateName :: Lens' UpdateServerCertificate Text
updServerCertificateName = lens _updServerCertificateName (\ s a -> s{_updServerCertificateName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest UpdateServerCertificate where
        type Sv UpdateServerCertificate = IAM
        type Rs UpdateServerCertificate =
             UpdateServerCertificateResponse
        request = post
        response
          = receiveNull UpdateServerCertificateResponse'

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
                 _updNewServerCertificateName,
               "NewPath" =: _updNewPath,
               "ServerCertificateName" =: _updServerCertificateName]

-- | /See:/ 'updateServerCertificateResponse' smart constructor.
data UpdateServerCertificateResponse = UpdateServerCertificateResponse' deriving (Eq, Read, Show)

-- | 'UpdateServerCertificateResponse' smart constructor.
updateServerCertificateResponse :: UpdateServerCertificateResponse
updateServerCertificateResponse = UpdateServerCertificateResponse';
