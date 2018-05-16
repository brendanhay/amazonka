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
-- Module      : Network.AWS.IoT.UpdateCACertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered CA certificate.
--
--
module Network.AWS.IoT.UpdateCACertificate
    (
    -- * Creating a Request
      updateCACertificate
    , UpdateCACertificate
    -- * Request Lenses
    , ucacRemoveAutoRegistration
    , ucacNewStatus
    , ucacRegistrationConfig
    , ucacNewAutoRegistrationStatus
    , ucacCertificateId

    -- * Destructuring the Response
    , updateCACertificateResponse
    , UpdateCACertificateResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the UpdateCACertificate operation.
--
--
--
-- /See:/ 'updateCACertificate' smart constructor.
data UpdateCACertificate = UpdateCACertificate'
  { _ucacRemoveAutoRegistration    :: !(Maybe Bool)
  , _ucacNewStatus                 :: !(Maybe CACertificateStatus)
  , _ucacRegistrationConfig        :: !(Maybe RegistrationConfig)
  , _ucacNewAutoRegistrationStatus :: !(Maybe AutoRegistrationStatus)
  , _ucacCertificateId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucacRemoveAutoRegistration' - If true, remove auto registration.
--
-- * 'ucacNewStatus' - The updated status of the CA certificate. __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- * 'ucacRegistrationConfig' - Information about the registration configuration.
--
-- * 'ucacNewAutoRegistrationStatus' - The new value for the auto registration status. Valid values are: "ENABLE" or "DISABLE".
--
-- * 'ucacCertificateId' - The CA certificate identifier.
updateCACertificate
    :: Text -- ^ 'ucacCertificateId'
    -> UpdateCACertificate
updateCACertificate pCertificateId_ =
  UpdateCACertificate'
    { _ucacRemoveAutoRegistration = Nothing
    , _ucacNewStatus = Nothing
    , _ucacRegistrationConfig = Nothing
    , _ucacNewAutoRegistrationStatus = Nothing
    , _ucacCertificateId = pCertificateId_
    }


-- | If true, remove auto registration.
ucacRemoveAutoRegistration :: Lens' UpdateCACertificate (Maybe Bool)
ucacRemoveAutoRegistration = lens _ucacRemoveAutoRegistration (\ s a -> s{_ucacRemoveAutoRegistration = a})

-- | The updated status of the CA certificate. __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
ucacNewStatus :: Lens' UpdateCACertificate (Maybe CACertificateStatus)
ucacNewStatus = lens _ucacNewStatus (\ s a -> s{_ucacNewStatus = a})

-- | Information about the registration configuration.
ucacRegistrationConfig :: Lens' UpdateCACertificate (Maybe RegistrationConfig)
ucacRegistrationConfig = lens _ucacRegistrationConfig (\ s a -> s{_ucacRegistrationConfig = a})

-- | The new value for the auto registration status. Valid values are: "ENABLE" or "DISABLE".
ucacNewAutoRegistrationStatus :: Lens' UpdateCACertificate (Maybe AutoRegistrationStatus)
ucacNewAutoRegistrationStatus = lens _ucacNewAutoRegistrationStatus (\ s a -> s{_ucacNewAutoRegistrationStatus = a})

-- | The CA certificate identifier.
ucacCertificateId :: Lens' UpdateCACertificate Text
ucacCertificateId = lens _ucacCertificateId (\ s a -> s{_ucacCertificateId = a})

instance AWSRequest UpdateCACertificate where
        type Rs UpdateCACertificate =
             UpdateCACertificateResponse
        request = putJSON ioT
        response = receiveNull UpdateCACertificateResponse'

instance Hashable UpdateCACertificate where

instance NFData UpdateCACertificate where

instance ToHeaders UpdateCACertificate where
        toHeaders = const mempty

instance ToJSON UpdateCACertificate where
        toJSON UpdateCACertificate'{..}
          = object
              (catMaybes
                 [("removeAutoRegistration" .=) <$>
                    _ucacRemoveAutoRegistration,
                  ("registrationConfig" .=) <$>
                    _ucacRegistrationConfig])

instance ToPath UpdateCACertificate where
        toPath UpdateCACertificate'{..}
          = mconcat
              ["/cacertificate/", toBS _ucacCertificateId]

instance ToQuery UpdateCACertificate where
        toQuery UpdateCACertificate'{..}
          = mconcat
              ["newStatus" =: _ucacNewStatus,
               "newAutoRegistrationStatus" =:
                 _ucacNewAutoRegistrationStatus]

-- | /See:/ 'updateCACertificateResponse' smart constructor.
data UpdateCACertificateResponse =
  UpdateCACertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCACertificateResponse' with the minimum fields required to make a request.
--
updateCACertificateResponse
    :: UpdateCACertificateResponse
updateCACertificateResponse = UpdateCACertificateResponse'


instance NFData UpdateCACertificateResponse where
