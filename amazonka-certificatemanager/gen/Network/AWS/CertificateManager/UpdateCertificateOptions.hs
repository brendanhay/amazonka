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
-- Module      : Network.AWS.CertificateManager.UpdateCertificateOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a certificate. Currently, you can use this function to specify whether to opt in to or out of recording your certificate in a certificate transparency log. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
--
--
module Network.AWS.CertificateManager.UpdateCertificateOptions
    (
    -- * Creating a Request
      updateCertificateOptions
    , UpdateCertificateOptions
    -- * Request Lenses
    , ucoCertificateARN
    , ucoOptions

    -- * Destructuring the Response
    , updateCertificateOptionsResponse
    , UpdateCertificateOptionsResponse
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCertificateOptions' smart constructor.
data UpdateCertificateOptions = UpdateCertificateOptions'
  { _ucoCertificateARN :: !Text
  , _ucoOptions        :: !CertificateOptions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCertificateOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucoCertificateARN' - ARN of the requested certificate to update. This must be of the form: @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @
--
-- * 'ucoOptions' - Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser.
updateCertificateOptions
    :: Text -- ^ 'ucoCertificateARN'
    -> CertificateOptions -- ^ 'ucoOptions'
    -> UpdateCertificateOptions
updateCertificateOptions pCertificateARN_ pOptions_ =
  UpdateCertificateOptions'
    {_ucoCertificateARN = pCertificateARN_, _ucoOptions = pOptions_}


-- | ARN of the requested certificate to update. This must be of the form: @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @
ucoCertificateARN :: Lens' UpdateCertificateOptions Text
ucoCertificateARN = lens _ucoCertificateARN (\ s a -> s{_ucoCertificateARN = a})

-- | Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser.
ucoOptions :: Lens' UpdateCertificateOptions CertificateOptions
ucoOptions = lens _ucoOptions (\ s a -> s{_ucoOptions = a})

instance AWSRequest UpdateCertificateOptions where
        type Rs UpdateCertificateOptions =
             UpdateCertificateOptionsResponse
        request = postJSON certificateManager
        response
          = receiveNull UpdateCertificateOptionsResponse'

instance Hashable UpdateCertificateOptions where

instance NFData UpdateCertificateOptions where

instance ToHeaders UpdateCertificateOptions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.UpdateCertificateOptions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCertificateOptions where
        toJSON UpdateCertificateOptions'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _ucoCertificateARN),
                  Just ("Options" .= _ucoOptions)])

instance ToPath UpdateCertificateOptions where
        toPath = const "/"

instance ToQuery UpdateCertificateOptions where
        toQuery = const mempty

-- | /See:/ 'updateCertificateOptionsResponse' smart constructor.
data UpdateCertificateOptionsResponse =
  UpdateCertificateOptionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCertificateOptionsResponse' with the minimum fields required to make a request.
--
updateCertificateOptionsResponse
    :: UpdateCertificateOptionsResponse
updateCertificateOptionsResponse = UpdateCertificateOptionsResponse'


instance NFData UpdateCertificateOptionsResponse
         where
