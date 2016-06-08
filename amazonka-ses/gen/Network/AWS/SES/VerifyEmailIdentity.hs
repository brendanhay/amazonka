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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an email address. This action causes a confirmation email message to be sent to the specified address.
--
-- This action is throttled at one request per second.
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
    , veirsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'verifyEmailIdentity' smart constructor.
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
        request = postQuery ses
        response
          = receiveXMLWrapper "VerifyEmailIdentityResult"
              (\ s h x ->
                 VerifyEmailIdentityResponse' <$> (pure (fromEnum s)))

instance Hashable VerifyEmailIdentity

instance NFData VerifyEmailIdentity

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
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse'
    { _veirsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VerifyEmailIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veirsResponseStatus'
verifyEmailIdentityResponse
    :: Int -- ^ 'veirsResponseStatus'
    -> VerifyEmailIdentityResponse
verifyEmailIdentityResponse pResponseStatus_ =
    VerifyEmailIdentityResponse'
    { _veirsResponseStatus = pResponseStatus_
    }

-- | The response status code.
veirsResponseStatus :: Lens' VerifyEmailIdentityResponse Int
veirsResponseStatus = lens _veirsResponseStatus (\ s a -> s{_veirsResponseStatus = a});

instance NFData VerifyEmailIdentityResponse
