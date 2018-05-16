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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates identity provider information for a user pool.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
    (
    -- * Creating a Request
      updateIdentityProvider
    , UpdateIdentityProvider
    -- * Request Lenses
    , uipIdpIdentifiers
    , uipAttributeMapping
    , uipProviderDetails
    , uipUserPoolId
    , uipProviderName

    -- * Destructuring the Response
    , updateIdentityProviderResponse
    , UpdateIdentityProviderResponse
    -- * Response Lenses
    , uiprsResponseStatus
    , uiprsIdentityProvider
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { _uipIdpIdentifiers   :: !(Maybe [Text])
  , _uipAttributeMapping :: !(Maybe (Map Text Text))
  , _uipProviderDetails  :: !(Maybe (Map Text Text))
  , _uipUserPoolId       :: !Text
  , _uipProviderName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIdentityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uipIdpIdentifiers' - A list of identity provider identifiers.
--
-- * 'uipAttributeMapping' - The identity provider attribute mapping to be changed.
--
-- * 'uipProviderDetails' - The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
--
-- * 'uipUserPoolId' - The user pool ID.
--
-- * 'uipProviderName' - The identity provider name.
updateIdentityProvider
    :: Text -- ^ 'uipUserPoolId'
    -> Text -- ^ 'uipProviderName'
    -> UpdateIdentityProvider
updateIdentityProvider pUserPoolId_ pProviderName_ =
  UpdateIdentityProvider'
    { _uipIdpIdentifiers = Nothing
    , _uipAttributeMapping = Nothing
    , _uipProviderDetails = Nothing
    , _uipUserPoolId = pUserPoolId_
    , _uipProviderName = pProviderName_
    }


-- | A list of identity provider identifiers.
uipIdpIdentifiers :: Lens' UpdateIdentityProvider [Text]
uipIdpIdentifiers = lens _uipIdpIdentifiers (\ s a -> s{_uipIdpIdentifiers = a}) . _Default . _Coerce

-- | The identity provider attribute mapping to be changed.
uipAttributeMapping :: Lens' UpdateIdentityProvider (HashMap Text Text)
uipAttributeMapping = lens _uipAttributeMapping (\ s a -> s{_uipAttributeMapping = a}) . _Default . _Map

-- | The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
uipProviderDetails :: Lens' UpdateIdentityProvider (HashMap Text Text)
uipProviderDetails = lens _uipProviderDetails (\ s a -> s{_uipProviderDetails = a}) . _Default . _Map

-- | The user pool ID.
uipUserPoolId :: Lens' UpdateIdentityProvider Text
uipUserPoolId = lens _uipUserPoolId (\ s a -> s{_uipUserPoolId = a})

-- | The identity provider name.
uipProviderName :: Lens' UpdateIdentityProvider Text
uipProviderName = lens _uipProviderName (\ s a -> s{_uipProviderName = a})

instance AWSRequest UpdateIdentityProvider where
        type Rs UpdateIdentityProvider =
             UpdateIdentityProviderResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateIdentityProviderResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "IdentityProvider"))

instance Hashable UpdateIdentityProvider where

instance NFData UpdateIdentityProvider where

instance ToHeaders UpdateIdentityProvider where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateIdentityProvider"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateIdentityProvider where
        toJSON UpdateIdentityProvider'{..}
          = object
              (catMaybes
                 [("IdpIdentifiers" .=) <$> _uipIdpIdentifiers,
                  ("AttributeMapping" .=) <$> _uipAttributeMapping,
                  ("ProviderDetails" .=) <$> _uipProviderDetails,
                  Just ("UserPoolId" .= _uipUserPoolId),
                  Just ("ProviderName" .= _uipProviderName)])

instance ToPath UpdateIdentityProvider where
        toPath = const "/"

instance ToQuery UpdateIdentityProvider where
        toQuery = const mempty

-- | /See:/ 'updateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { _uiprsResponseStatus   :: !Int
  , _uiprsIdentityProvider :: !IdentityProviderType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIdentityProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiprsResponseStatus' - -- | The response status code.
--
-- * 'uiprsIdentityProvider' - The identity provider object.
updateIdentityProviderResponse
    :: Int -- ^ 'uiprsResponseStatus'
    -> IdentityProviderType -- ^ 'uiprsIdentityProvider'
    -> UpdateIdentityProviderResponse
updateIdentityProviderResponse pResponseStatus_ pIdentityProvider_ =
  UpdateIdentityProviderResponse'
    { _uiprsResponseStatus = pResponseStatus_
    , _uiprsIdentityProvider = pIdentityProvider_
    }


-- | -- | The response status code.
uiprsResponseStatus :: Lens' UpdateIdentityProviderResponse Int
uiprsResponseStatus = lens _uiprsResponseStatus (\ s a -> s{_uiprsResponseStatus = a})

-- | The identity provider object.
uiprsIdentityProvider :: Lens' UpdateIdentityProviderResponse IdentityProviderType
uiprsIdentityProvider = lens _uiprsIdentityProvider (\ s a -> s{_uiprsIdentityProvider = a})

instance NFData UpdateIdentityProviderResponse where
