{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.CreateHSMClientCertificate
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

-- | Creates an HSM client certificate that an Amazon Redshift cluster will
-- use to connect to the client\'s HSM in order to store and retrieve the
-- keys used to encrypt the cluster databases.
--
-- The command returns a public key, which you must store in the HSM. In
-- addition to creating the HSM certificate, you must create an Amazon
-- Redshift HSM configuration that provides a cluster the information
-- needed to store and use encryption keys in the HSM. For more
-- information, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules>
-- in the Amazon Redshift Cluster Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateHSMClientCertificate.html>
module Network.AWS.Redshift.CreateHSMClientCertificate
    (
    -- * Request
      CreateHSMClientCertificate
    -- ** Request constructor
    , createHSMClientCertificate
    -- ** Request lenses
    , chccTags
    , chccHSMClientCertificateIdentifier

    -- * Response
    , CreateHSMClientCertificateResponse
    -- ** Response constructor
    , createHSMClientCertificateResponse
    -- ** Response lenses
    , chccrHSMClientCertificate
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'createHSMClientCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccTags'
--
-- * 'chccHSMClientCertificateIdentifier'
data CreateHSMClientCertificate = CreateHSMClientCertificate'{_chccTags :: [Tag], _chccHSMClientCertificateIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'CreateHSMClientCertificate' smart constructor.
createHSMClientCertificate :: Text -> CreateHSMClientCertificate
createHSMClientCertificate pHSMClientCertificateIdentifier = CreateHSMClientCertificate'{_chccTags = mempty, _chccHSMClientCertificateIdentifier = pHSMClientCertificateIdentifier};

-- | A list of tag instances.
chccTags :: Lens' CreateHSMClientCertificate [Tag]
chccTags = lens _chccTags (\ s a -> s{_chccTags = a});

-- | The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption
-- keys.
chccHSMClientCertificateIdentifier :: Lens' CreateHSMClientCertificate Text
chccHSMClientCertificateIdentifier = lens _chccHSMClientCertificateIdentifier (\ s a -> s{_chccHSMClientCertificateIdentifier = a});

instance AWSRequest CreateHSMClientCertificate where
        type Sv CreateHSMClientCertificate = Redshift
        type Rs CreateHSMClientCertificate =
             CreateHSMClientCertificateResponse
        request = post
        response
          = receiveXMLWrapper
              "CreateHsmClientCertificateResult"
              (\ s h x ->
                 CreateHSMClientCertificateResponse' <$>
                   x .@? "HsmClientCertificate")

instance ToHeaders CreateHSMClientCertificate where
        toHeaders = const mempty

instance ToPath CreateHSMClientCertificate where
        toPath = const "/"

instance ToQuery CreateHSMClientCertificate where
        toQuery CreateHSMClientCertificate'{..}
          = mconcat
              ["Action" =:
                 ("CreateHSMClientCertificate" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: "Tag" =: _chccTags,
               "HsmClientCertificateIdentifier" =:
                 _chccHSMClientCertificateIdentifier]

-- | /See:/ 'createHSMClientCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccrHSMClientCertificate'
newtype CreateHSMClientCertificateResponse = CreateHSMClientCertificateResponse'{_chccrHSMClientCertificate :: Maybe HSMClientCertificate} deriving (Eq, Read, Show)

-- | 'CreateHSMClientCertificateResponse' smart constructor.
createHSMClientCertificateResponse :: CreateHSMClientCertificateResponse
createHSMClientCertificateResponse = CreateHSMClientCertificateResponse'{_chccrHSMClientCertificate = Nothing};

-- | FIXME: Undocumented member.
chccrHSMClientCertificate :: Lens' CreateHSMClientCertificateResponse (Maybe HSMClientCertificate)
chccrHSMClientCertificate = lens _chccrHSMClientCertificate (\ s a -> s{_chccrHSMClientCertificate = a});
