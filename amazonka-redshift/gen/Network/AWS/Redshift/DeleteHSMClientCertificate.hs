{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteHSMClientCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM client certificate.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteHSMClientCertificate.html AWS API Reference> for DeleteHSMClientCertificate.
module Network.AWS.Redshift.DeleteHSMClientCertificate
    (
    -- * Creating a Request
      DeleteHSMClientCertificate
    , deleteHSMClientCertificate
    -- * Request Lenses
    , dhsmccHSMClientCertificateIdentifier

    -- * Destructuring the Response
    , DeleteHSMClientCertificateResponse
    , deleteHSMClientCertificateResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteHSMClientCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhsmccHSMClientCertificateIdentifier'
newtype DeleteHSMClientCertificate = DeleteHSMClientCertificate'
    { _dhsmccHSMClientCertificateIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHSMClientCertificate' smart constructor.
deleteHSMClientCertificate :: Text -> DeleteHSMClientCertificate
deleteHSMClientCertificate pHSMClientCertificateIdentifier_ =
    DeleteHSMClientCertificate'
    { _dhsmccHSMClientCertificateIdentifier = pHSMClientCertificateIdentifier_
    }

-- | The identifier of the HSM client certificate to be deleted.
dhsmccHSMClientCertificateIdentifier :: Lens' DeleteHSMClientCertificate Text
dhsmccHSMClientCertificateIdentifier = lens _dhsmccHSMClientCertificateIdentifier (\ s a -> s{_dhsmccHSMClientCertificateIdentifier = a});

instance AWSRequest DeleteHSMClientCertificate where
        type Sv DeleteHSMClientCertificate = Redshift
        type Rs DeleteHSMClientCertificate =
             DeleteHSMClientCertificateResponse
        request = postQuery
        response
          = receiveNull DeleteHSMClientCertificateResponse'

instance ToHeaders DeleteHSMClientCertificate where
        toHeaders = const mempty

instance ToPath DeleteHSMClientCertificate where
        toPath = const "/"

instance ToQuery DeleteHSMClientCertificate where
        toQuery DeleteHSMClientCertificate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteHsmClientCertificate" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "HsmClientCertificateIdentifier" =:
                 _dhsmccHSMClientCertificateIdentifier]

-- | /See:/ 'deleteHSMClientCertificateResponse' smart constructor.
data DeleteHSMClientCertificateResponse =
    DeleteHSMClientCertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHSMClientCertificateResponse' smart constructor.
deleteHSMClientCertificateResponse :: DeleteHSMClientCertificateResponse
deleteHSMClientCertificateResponse = DeleteHSMClientCertificateResponse'
