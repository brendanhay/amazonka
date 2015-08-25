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
-- Module      : Network.AWS.SES.VerifyEmailIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an email address. This action causes a confirmation email
-- message to be sent to the specified address.
--
-- This action is throttled at one request per second.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyEmailIdentity.html AWS API Reference> for VerifyEmailIdentity.
module Network.AWS.SES.VerifyEmailIdentity
    (
    -- * Creating a Request
      verifyEmailIdentity
    , VerifyEmailIdentity
    -- * Request Lenses
    , veiEmailAddress

    -- * Destructuring the Response
    , verifyEmailIdentityResponse
    , VerifyEmailIdentityResponse
    -- * Response Lenses
    , veirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request instructing the service to begin email address
-- verification.
--
-- /See:/ 'verifyEmailIdentity' smart constructor.
newtype VerifyEmailIdentity = VerifyEmailIdentity'
    { _veiEmailAddress :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VerifyEmailIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veiEmailAddress'
verifyEmailIdentity
    :: Text -- ^ 'veiEmailAddress'
    -> VerifyEmailIdentity
verifyEmailIdentity pEmailAddress_ =
    VerifyEmailIdentity'
    { _veiEmailAddress = pEmailAddress_
    }

-- | The email address to be verified.
veiEmailAddress :: Lens' VerifyEmailIdentity Text
veiEmailAddress = lens _veiEmailAddress (\ s a -> s{_veiEmailAddress = a});

instance AWSRequest VerifyEmailIdentity where
        type Rs VerifyEmailIdentity =
             VerifyEmailIdentityResponse
        request = postQuery sES
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
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse'
    { _veirsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VerifyEmailIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veirsStatus'
verifyEmailIdentityResponse
    :: Int -- ^ 'veirsStatus'
    -> VerifyEmailIdentityResponse
verifyEmailIdentityResponse pStatus_ =
    VerifyEmailIdentityResponse'
    { _veirsStatus = pStatus_
    }

-- | The response status code.
veirsStatus :: Lens' VerifyEmailIdentityResponse Int
veirsStatus = lens _veirsStatus (\ s a -> s{_veirsStatus = a});
