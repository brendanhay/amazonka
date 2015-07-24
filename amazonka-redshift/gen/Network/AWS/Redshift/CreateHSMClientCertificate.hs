{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateHSMClientCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM client certificate that an Amazon Redshift cluster will
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
    , chccrsHSMClientCertificate
    , chccrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createHSMClientCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccTags'
--
-- * 'chccHSMClientCertificateIdentifier'
data CreateHSMClientCertificate = CreateHSMClientCertificate'
    { _chccTags                           :: !(Maybe [Tag])
    , _chccHSMClientCertificateIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHSMClientCertificate' smart constructor.
createHSMClientCertificate :: Text -> CreateHSMClientCertificate
createHSMClientCertificate pHSMClientCertificateIdentifier_ =
    CreateHSMClientCertificate'
    { _chccTags = Nothing
    , _chccHSMClientCertificateIdentifier = pHSMClientCertificateIdentifier_
    }

-- | A list of tag instances.
chccTags :: Lens' CreateHSMClientCertificate [Tag]
chccTags = lens _chccTags (\ s a -> s{_chccTags = a}) . _Default;

-- | The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption
-- keys.
chccHSMClientCertificateIdentifier :: Lens' CreateHSMClientCertificate Text
chccHSMClientCertificateIdentifier = lens _chccHSMClientCertificateIdentifier (\ s a -> s{_chccHSMClientCertificateIdentifier = a});

instance AWSRequest CreateHSMClientCertificate where
        type Sv CreateHSMClientCertificate = Redshift
        type Rs CreateHSMClientCertificate =
             CreateHSMClientCertificateResponse
        request = post "CreateHSMClientCertificate"
        response
          = receiveXMLWrapper
              "CreateHsmClientCertificateResult"
              (\ s h x ->
                 CreateHSMClientCertificateResponse' <$>
                   (x .@? "HsmClientCertificate") <*>
                     (pure (fromEnum s)))

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
               "Tags" =: toQuery (toQueryList "Tag" <$> _chccTags),
               "HsmClientCertificateIdentifier" =:
                 _chccHSMClientCertificateIdentifier]

-- | /See:/ 'createHSMClientCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccrsHSMClientCertificate'
--
-- * 'chccrsStatus'
data CreateHSMClientCertificateResponse = CreateHSMClientCertificateResponse'
    { _chccrsHSMClientCertificate :: !(Maybe HSMClientCertificate)
    , _chccrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHSMClientCertificateResponse' smart constructor.
createHSMClientCertificateResponse :: Int -> CreateHSMClientCertificateResponse
createHSMClientCertificateResponse pStatus_ =
    CreateHSMClientCertificateResponse'
    { _chccrsHSMClientCertificate = Nothing
    , _chccrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
chccrsHSMClientCertificate :: Lens' CreateHSMClientCertificateResponse (Maybe HSMClientCertificate)
chccrsHSMClientCertificate = lens _chccrsHSMClientCertificate (\ s a -> s{_chccrsHSMClientCertificate = a});

-- | FIXME: Undocumented member.
chccrsStatus :: Lens' CreateHSMClientCertificateResponse Int
chccrsStatus = lens _chccrsStatus (\ s a -> s{_chccrsStatus = a});
