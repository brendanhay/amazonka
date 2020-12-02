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
-- Module      : Network.AWS.CognitoIdentityProvider.AddCustomAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional user attributes to the user pool schema.
--
--
module Network.AWS.CognitoIdentityProvider.AddCustomAttributes
    (
    -- * Creating a Request
      addCustomAttributes
    , AddCustomAttributes
    -- * Request Lenses
    , acaUserPoolId
    , acaCustomAttributes

    -- * Destructuring the Response
    , addCustomAttributesResponse
    , AddCustomAttributesResponse
    -- * Response Lenses
    , acarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to add custom attributes.
--
--
--
-- /See:/ 'addCustomAttributes' smart constructor.
data AddCustomAttributes = AddCustomAttributes'
  { _acaUserPoolId       :: !Text
  , _acaCustomAttributes :: !(List1 SchemaAttributeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddCustomAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acaUserPoolId' - The user pool ID for the user pool where you want to add custom attributes.
--
-- * 'acaCustomAttributes' - An array of custom attributes, such as Mutable and Name.
addCustomAttributes
    :: Text -- ^ 'acaUserPoolId'
    -> NonEmpty SchemaAttributeType -- ^ 'acaCustomAttributes'
    -> AddCustomAttributes
addCustomAttributes pUserPoolId_ pCustomAttributes_ =
  AddCustomAttributes'
    { _acaUserPoolId = pUserPoolId_
    , _acaCustomAttributes = _List1 # pCustomAttributes_
    }


-- | The user pool ID for the user pool where you want to add custom attributes.
acaUserPoolId :: Lens' AddCustomAttributes Text
acaUserPoolId = lens _acaUserPoolId (\ s a -> s{_acaUserPoolId = a})

-- | An array of custom attributes, such as Mutable and Name.
acaCustomAttributes :: Lens' AddCustomAttributes (NonEmpty SchemaAttributeType)
acaCustomAttributes = lens _acaCustomAttributes (\ s a -> s{_acaCustomAttributes = a}) . _List1

instance AWSRequest AddCustomAttributes where
        type Rs AddCustomAttributes =
             AddCustomAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AddCustomAttributesResponse' <$> (pure (fromEnum s)))

instance Hashable AddCustomAttributes where

instance NFData AddCustomAttributes where

instance ToHeaders AddCustomAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AddCustomAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddCustomAttributes where
        toJSON AddCustomAttributes'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _acaUserPoolId),
                  Just ("CustomAttributes" .= _acaCustomAttributes)])

instance ToPath AddCustomAttributes where
        toPath = const "/"

instance ToQuery AddCustomAttributes where
        toQuery = const mempty

-- | Represents the response from the server for the request to add custom attributes.
--
--
--
-- /See:/ 'addCustomAttributesResponse' smart constructor.
newtype AddCustomAttributesResponse = AddCustomAttributesResponse'
  { _acarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddCustomAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acarsResponseStatus' - -- | The response status code.
addCustomAttributesResponse
    :: Int -- ^ 'acarsResponseStatus'
    -> AddCustomAttributesResponse
addCustomAttributesResponse pResponseStatus_ =
  AddCustomAttributesResponse' {_acarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
acarsResponseStatus :: Lens' AddCustomAttributesResponse Int
acarsResponseStatus = lens _acarsResponseStatus (\ s a -> s{_acarsResponseStatus = a})

instance NFData AddCustomAttributesResponse where
