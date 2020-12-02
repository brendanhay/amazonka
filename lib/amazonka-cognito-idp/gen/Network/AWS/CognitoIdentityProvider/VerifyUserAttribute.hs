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
-- Module      : Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the specified user attributes in the user pool.
--
--
module Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
    (
    -- * Creating a Request
      verifyUserAttribute
    , VerifyUserAttribute
    -- * Request Lenses
    , vuaAccessToken
    , vuaAttributeName
    , vuaCode

    -- * Destructuring the Response
    , verifyUserAttributeResponse
    , VerifyUserAttributeResponse
    -- * Response Lenses
    , vuarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to verify user attributes.
--
--
--
-- /See:/ 'verifyUserAttribute' smart constructor.
data VerifyUserAttribute = VerifyUserAttribute'
  { _vuaAccessToken   :: !(Sensitive Text)
  , _vuaAttributeName :: !Text
  , _vuaCode          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyUserAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vuaAccessToken' - Represents the access token of the request to verify user attributes.
--
-- * 'vuaAttributeName' - The attribute name in the request to verify user attributes.
--
-- * 'vuaCode' - The verification code in the request to verify user attributes.
verifyUserAttribute
    :: Text -- ^ 'vuaAccessToken'
    -> Text -- ^ 'vuaAttributeName'
    -> Text -- ^ 'vuaCode'
    -> VerifyUserAttribute
verifyUserAttribute pAccessToken_ pAttributeName_ pCode_ =
  VerifyUserAttribute'
    { _vuaAccessToken = _Sensitive # pAccessToken_
    , _vuaAttributeName = pAttributeName_
    , _vuaCode = pCode_
    }


-- | Represents the access token of the request to verify user attributes.
vuaAccessToken :: Lens' VerifyUserAttribute Text
vuaAccessToken = lens _vuaAccessToken (\ s a -> s{_vuaAccessToken = a}) . _Sensitive

-- | The attribute name in the request to verify user attributes.
vuaAttributeName :: Lens' VerifyUserAttribute Text
vuaAttributeName = lens _vuaAttributeName (\ s a -> s{_vuaAttributeName = a})

-- | The verification code in the request to verify user attributes.
vuaCode :: Lens' VerifyUserAttribute Text
vuaCode = lens _vuaCode (\ s a -> s{_vuaCode = a})

instance AWSRequest VerifyUserAttribute where
        type Rs VerifyUserAttribute =
             VerifyUserAttributeResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 VerifyUserAttributeResponse' <$> (pure (fromEnum s)))

instance Hashable VerifyUserAttribute where

instance NFData VerifyUserAttribute where

instance ToHeaders VerifyUserAttribute where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.VerifyUserAttribute"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON VerifyUserAttribute where
        toJSON VerifyUserAttribute'{..}
          = object
              (catMaybes
                 [Just ("AccessToken" .= _vuaAccessToken),
                  Just ("AttributeName" .= _vuaAttributeName),
                  Just ("Code" .= _vuaCode)])

instance ToPath VerifyUserAttribute where
        toPath = const "/"

instance ToQuery VerifyUserAttribute where
        toQuery = const mempty

-- | A container representing the response from the server from the request to verify user attributes.
--
--
--
-- /See:/ 'verifyUserAttributeResponse' smart constructor.
newtype VerifyUserAttributeResponse = VerifyUserAttributeResponse'
  { _vuarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyUserAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vuarsResponseStatus' - -- | The response status code.
verifyUserAttributeResponse
    :: Int -- ^ 'vuarsResponseStatus'
    -> VerifyUserAttributeResponse
verifyUserAttributeResponse pResponseStatus_ =
  VerifyUserAttributeResponse' {_vuarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
vuarsResponseStatus :: Lens' VerifyUserAttributeResponse Int
vuarsResponseStatus = lens _vuarsResponseStatus (\ s a -> s{_vuarsResponseStatus = a})

instance NFData VerifyUserAttributeResponse where
