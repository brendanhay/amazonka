{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.VerifyEmailIdentity
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

-- | Verifies an email address. This action causes a confirmation email
-- message to be sent to the specified address.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyEmailIdentity.html>
module Network.AWS.SES.VerifyEmailIdentity
    (
    -- * Request
      VerifyEmailIdentity
    -- ** Request constructor
    , verifyEmailIdentity
    -- ** Request lenses
    , veiEmailAddress

    -- * Response
    , VerifyEmailIdentityResponse
    -- ** Response constructor
    , verifyEmailIdentityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types

-- | /See:/ 'verifyEmailIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veiEmailAddress'
newtype VerifyEmailIdentity = VerifyEmailIdentity'{_veiEmailAddress :: Text} deriving (Eq, Read, Show)

-- | 'VerifyEmailIdentity' smart constructor.
verifyEmailIdentity :: Text -> VerifyEmailIdentity
verifyEmailIdentity pEmailAddress = VerifyEmailIdentity'{_veiEmailAddress = pEmailAddress};

-- | The email address to be verified.
veiEmailAddress :: Lens' VerifyEmailIdentity Text
veiEmailAddress = lens _veiEmailAddress (\ s a -> s{_veiEmailAddress = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest VerifyEmailIdentity where
        type Sv VerifyEmailIdentity = SES
        type Rs VerifyEmailIdentity =
             VerifyEmailIdentityResponse
        request = post
        response = receiveNull VerifyEmailIdentityResponse'

instance ToHeaders VerifyEmailIdentity where
        toHeaders = const mempty

instance ToPath VerifyEmailIdentity where
        toPath = const "/"

instance ToQuery VerifyEmailIdentity where
        toQuery VerifyEmailIdentity'{..}
          = mconcat
              ["Action" =: ("VerifyEmailIdentity" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EmailAddress" =: _veiEmailAddress]

-- | /See:/ 'verifyEmailIdentityResponse' smart constructor.
data VerifyEmailIdentityResponse = VerifyEmailIdentityResponse' deriving (Eq, Read, Show)

-- | 'VerifyEmailIdentityResponse' smart constructor.
verifyEmailIdentityResponse :: VerifyEmailIdentityResponse
verifyEmailIdentityResponse = VerifyEmailIdentityResponse';
