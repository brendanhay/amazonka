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
-- Module      : Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified identity provider.
--
--
module Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
    (
    -- * Creating a Request
      getIdentityProviderByIdentifier
    , GetIdentityProviderByIdentifier
    -- * Request Lenses
    , gipbiUserPoolId
    , gipbiIdpIdentifier

    -- * Destructuring the Response
    , getIdentityProviderByIdentifierResponse
    , GetIdentityProviderByIdentifierResponse
    -- * Response Lenses
    , gipbirsResponseStatus
    , gipbirsIdentityProvider
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIdentityProviderByIdentifier' smart constructor.
data GetIdentityProviderByIdentifier = GetIdentityProviderByIdentifier'
  { _gipbiUserPoolId    :: !Text
  , _gipbiIdpIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityProviderByIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipbiUserPoolId' - The user pool ID.
--
-- * 'gipbiIdpIdentifier' - The identity provider ID.
getIdentityProviderByIdentifier
    :: Text -- ^ 'gipbiUserPoolId'
    -> Text -- ^ 'gipbiIdpIdentifier'
    -> GetIdentityProviderByIdentifier
getIdentityProviderByIdentifier pUserPoolId_ pIdpIdentifier_ =
  GetIdentityProviderByIdentifier'
    {_gipbiUserPoolId = pUserPoolId_, _gipbiIdpIdentifier = pIdpIdentifier_}


-- | The user pool ID.
gipbiUserPoolId :: Lens' GetIdentityProviderByIdentifier Text
gipbiUserPoolId = lens _gipbiUserPoolId (\ s a -> s{_gipbiUserPoolId = a})

-- | The identity provider ID.
gipbiIdpIdentifier :: Lens' GetIdentityProviderByIdentifier Text
gipbiIdpIdentifier = lens _gipbiIdpIdentifier (\ s a -> s{_gipbiIdpIdentifier = a})

instance AWSRequest GetIdentityProviderByIdentifier
         where
        type Rs GetIdentityProviderByIdentifier =
             GetIdentityProviderByIdentifierResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetIdentityProviderByIdentifierResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "IdentityProvider"))

instance Hashable GetIdentityProviderByIdentifier
         where

instance NFData GetIdentityProviderByIdentifier where

instance ToHeaders GetIdentityProviderByIdentifier
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetIdentityProviderByIdentifier"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetIdentityProviderByIdentifier where
        toJSON GetIdentityProviderByIdentifier'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _gipbiUserPoolId),
                  Just ("IdpIdentifier" .= _gipbiIdpIdentifier)])

instance ToPath GetIdentityProviderByIdentifier where
        toPath = const "/"

instance ToQuery GetIdentityProviderByIdentifier
         where
        toQuery = const mempty

-- | /See:/ 'getIdentityProviderByIdentifierResponse' smart constructor.
data GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse'
  { _gipbirsResponseStatus   :: !Int
  , _gipbirsIdentityProvider :: !IdentityProviderType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityProviderByIdentifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipbirsResponseStatus' - -- | The response status code.
--
-- * 'gipbirsIdentityProvider' - The identity provider object.
getIdentityProviderByIdentifierResponse
    :: Int -- ^ 'gipbirsResponseStatus'
    -> IdentityProviderType -- ^ 'gipbirsIdentityProvider'
    -> GetIdentityProviderByIdentifierResponse
getIdentityProviderByIdentifierResponse pResponseStatus_ pIdentityProvider_ =
  GetIdentityProviderByIdentifierResponse'
    { _gipbirsResponseStatus = pResponseStatus_
    , _gipbirsIdentityProvider = pIdentityProvider_
    }


-- | -- | The response status code.
gipbirsResponseStatus :: Lens' GetIdentityProviderByIdentifierResponse Int
gipbirsResponseStatus = lens _gipbirsResponseStatus (\ s a -> s{_gipbirsResponseStatus = a})

-- | The identity provider object.
gipbirsIdentityProvider :: Lens' GetIdentityProviderByIdentifierResponse IdentityProviderType
gipbirsIdentityProvider = lens _gipbirsIdentityProvider (\ s a -> s{_gipbirsIdentityProvider = a})

instance NFData
           GetIdentityProviderByIdentifierResponse
         where
