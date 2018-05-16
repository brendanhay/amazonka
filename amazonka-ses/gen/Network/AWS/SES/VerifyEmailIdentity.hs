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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a verification email is sent to the specified address.
--
--
-- You can execute this operation no more than once per second.
--
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
-- /See:/ 'verifyEmailIdentity' smart constructor.
newtype VerifyEmailIdentity = VerifyEmailIdentity'
  { _veiEmailAddress :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyEmailIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veiEmailAddress' - The email address to be verified.
verifyEmailIdentity
    :: Text -- ^ 'veiEmailAddress'
    -> VerifyEmailIdentity
verifyEmailIdentity pEmailAddress_ =
  VerifyEmailIdentity' {_veiEmailAddress = pEmailAddress_}


-- | The email address to be verified.
veiEmailAddress :: Lens' VerifyEmailIdentity Text
veiEmailAddress = lens _veiEmailAddress (\ s a -> s{_veiEmailAddress = a})

instance AWSRequest VerifyEmailIdentity where
        type Rs VerifyEmailIdentity =
             VerifyEmailIdentityResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "VerifyEmailIdentityResult"
              (\ s h x ->
                 VerifyEmailIdentityResponse' <$> (pure (fromEnum s)))

instance Hashable VerifyEmailIdentity where

instance NFData VerifyEmailIdentity where

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

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'verifyEmailIdentityResponse' smart constructor.
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse'
  { _veirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyEmailIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veirsResponseStatus' - -- | The response status code.
verifyEmailIdentityResponse
    :: Int -- ^ 'veirsResponseStatus'
    -> VerifyEmailIdentityResponse
verifyEmailIdentityResponse pResponseStatus_ =
  VerifyEmailIdentityResponse' {_veirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
veirsResponseStatus :: Lens' VerifyEmailIdentityResponse Int
veirsResponseStatus = lens _veirsResponseStatus (\ s a -> s{_veirsResponseStatus = a})

instance NFData VerifyEmailIdentityResponse where
