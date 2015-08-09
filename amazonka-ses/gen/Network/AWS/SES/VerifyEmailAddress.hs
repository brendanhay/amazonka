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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an email address. This action causes a confirmation email
-- message to be sent to the specified address.
--
-- The VerifyEmailAddress action is deprecated as of the May 15, 2012
-- release of Domain Verification. The VerifyEmailIdentity action is now
-- preferred.
--
-- This action is throttled at one request per second.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyEmailAddress.html AWS API Reference> for VerifyEmailAddress.
module Network.AWS.SES.VerifyEmailAddress
    (
    -- * Creating a Request
      VerifyEmailAddress
    , verifyEmailAddress
    -- * Request Lenses
    , veaEmailAddress

    -- * Destructuring the Response
    , VerifyEmailAddressResponse
    , verifyEmailAddressResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request instructing the service to begin email address
-- verification.
--
-- /See:/ 'verifyEmailAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veaEmailAddress'
newtype VerifyEmailAddress = VerifyEmailAddress'
    { _veaEmailAddress :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyEmailAddress' smart constructor.
verifyEmailAddress :: Text -> VerifyEmailAddress
verifyEmailAddress pEmailAddress_ =
    VerifyEmailAddress'
    { _veaEmailAddress = pEmailAddress_
    }

-- | The email address to be verified.
veaEmailAddress :: Lens' VerifyEmailAddress Text
veaEmailAddress = lens _veaEmailAddress (\ s a -> s{_veaEmailAddress = a});

instance AWSRequest VerifyEmailAddress where
        type Sv VerifyEmailAddress = SES
        type Rs VerifyEmailAddress =
             VerifyEmailAddressResponse
        request = postQuery
        response = receiveNull VerifyEmailAddressResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyEmailAddressResponse' smart constructor.
verifyEmailAddressResponse :: VerifyEmailAddressResponse
verifyEmailAddressResponse = VerifyEmailAddressResponse'
