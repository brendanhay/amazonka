{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.SES.VerifyEmailIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    -- ** Response lenses
    , veirStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to begin email address
-- verification.
--
-- /See:/ 'verifyEmailIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veiEmailAddress'
newtype VerifyEmailIdentity = VerifyEmailIdentity'
    { _veiEmailAddress :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyEmailIdentity' smart constructor.
verifyEmailIdentity :: Text -> VerifyEmailIdentity
verifyEmailIdentity pEmailAddress =
    VerifyEmailIdentity'
    { _veiEmailAddress = pEmailAddress
    }

-- | The email address to be verified.
veiEmailAddress :: Lens' VerifyEmailIdentity Text
veiEmailAddress = lens _veiEmailAddress (\ s a -> s{_veiEmailAddress = a});

instance AWSRequest VerifyEmailIdentity where
        type Sv VerifyEmailIdentity = SES
        type Rs VerifyEmailIdentity =
             VerifyEmailIdentityResponse
        request = post
        response
          = receiveXMLWrapper "VerifyEmailIdentityResult"
              (\ s h x ->
                 VerifyEmailIdentityResponse' <$> (pure (fromEnum s)))

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

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
--
-- /See:/ 'verifyEmailIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veirStatus'
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse'
    { _veirStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyEmailIdentityResponse' smart constructor.
verifyEmailIdentityResponse :: Int -> VerifyEmailIdentityResponse
verifyEmailIdentityResponse pStatus =
    VerifyEmailIdentityResponse'
    { _veirStatus = pStatus
    }

-- | FIXME: Undocumented member.
veirStatus :: Lens' VerifyEmailIdentityResponse Int
veirStatus = lens _veirStatus (\ s a -> s{_veirStatus = a});
