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
-- Module      : Network.AWS.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an email address. This action causes a confirmation email message to be sent to the specified address.
--
--
-- /Important:/ The VerifyEmailAddress action is deprecated as of the May 15, 2012 release of Domain Verification. The VerifyEmailIdentity action is now preferred.
--
-- This action is throttled at one request per second.
--
module Network.AWS.SES.VerifyEmailAddress
    (
    -- * Creating a Request
      verifyEmailAddress
    , VerifyEmailAddress
    -- * Request Lenses
    , veaEmailAddress

    -- * Destructuring the Response
    , verifyEmailAddressResponse
    , VerifyEmailAddressResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'verifyEmailAddress' smart constructor.
newtype VerifyEmailAddress = VerifyEmailAddress'
  { _veaEmailAddress :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyEmailAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veaEmailAddress' - The email address to be verified.
verifyEmailAddress
    :: Text -- ^ 'veaEmailAddress'
    -> VerifyEmailAddress
verifyEmailAddress pEmailAddress_ =
  VerifyEmailAddress' {_veaEmailAddress = pEmailAddress_}


-- | The email address to be verified.
veaEmailAddress :: Lens' VerifyEmailAddress Text
veaEmailAddress = lens _veaEmailAddress (\ s a -> s{_veaEmailAddress = a});

instance AWSRequest VerifyEmailAddress where
        type Rs VerifyEmailAddress =
             VerifyEmailAddressResponse
        request = postQuery ses
        response = receiveNull VerifyEmailAddressResponse'

instance Hashable VerifyEmailAddress where

instance NFData VerifyEmailAddress where

instance ToHeaders VerifyEmailAddress where
        toHeaders = const mempty

instance ToPath VerifyEmailAddress where
        toPath = const "/"

instance ToQuery VerifyEmailAddress where
        toQuery VerifyEmailAddress'{..}
          = mconcat
              ["Action" =: ("VerifyEmailAddress" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EmailAddress" =: _veaEmailAddress]

-- | /See:/ 'verifyEmailAddressResponse' smart constructor.
data VerifyEmailAddressResponse =
  VerifyEmailAddressResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyEmailAddressResponse' with the minimum fields required to make a request.
--
verifyEmailAddressResponse
    :: VerifyEmailAddressResponse
verifyEmailAddressResponse = VerifyEmailAddressResponse'


instance NFData VerifyEmailAddressResponse where
