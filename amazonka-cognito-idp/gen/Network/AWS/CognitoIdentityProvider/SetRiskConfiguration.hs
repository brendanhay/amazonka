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
-- Module      : Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures actions on detected risks. To delete the risk configuration for @UserPoolId@ or @ClientId@ , pass null values for all four configuration types.
--
--
-- To enable Amazon Cognito advanced security features, update the user pool to include the @UserPoolAddOns@ key@AdvancedSecurityMode@ .
--
-- See .
--
module Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
    (
    -- * Creating a Request
      setRiskConfiguration
    , SetRiskConfiguration
    -- * Request Lenses
    , srcRiskExceptionConfiguration
    , srcClientId
    , srcAccountTakeoverRiskConfiguration
    , srcCompromisedCredentialsRiskConfiguration
    , srcUserPoolId

    -- * Destructuring the Response
    , setRiskConfigurationResponse
    , SetRiskConfigurationResponse
    -- * Response Lenses
    , srcrsResponseStatus
    , srcrsRiskConfiguration
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setRiskConfiguration' smart constructor.
data SetRiskConfiguration = SetRiskConfiguration'
  { _srcRiskExceptionConfiguration :: !(Maybe RiskExceptionConfigurationType)
  , _srcClientId :: !(Maybe (Sensitive Text))
  , _srcAccountTakeoverRiskConfiguration :: !(Maybe AccountTakeoverRiskConfigurationType)
  , _srcCompromisedCredentialsRiskConfiguration :: !(Maybe CompromisedCredentialsRiskConfigurationType)
  , _srcUserPoolId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetRiskConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcRiskExceptionConfiguration' - The configuration to override the risk decision.
--
-- * 'srcClientId' - The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool. Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
--
-- * 'srcAccountTakeoverRiskConfiguration' - The account takeover risk configuration.
--
-- * 'srcCompromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration.
--
-- * 'srcUserPoolId' - The user pool ID.
setRiskConfiguration
    :: Text -- ^ 'srcUserPoolId'
    -> SetRiskConfiguration
setRiskConfiguration pUserPoolId_ =
  SetRiskConfiguration'
    { _srcRiskExceptionConfiguration = Nothing
    , _srcClientId = Nothing
    , _srcAccountTakeoverRiskConfiguration = Nothing
    , _srcCompromisedCredentialsRiskConfiguration = Nothing
    , _srcUserPoolId = pUserPoolId_
    }


-- | The configuration to override the risk decision.
srcRiskExceptionConfiguration :: Lens' SetRiskConfiguration (Maybe RiskExceptionConfigurationType)
srcRiskExceptionConfiguration = lens _srcRiskExceptionConfiguration (\ s a -> s{_srcRiskExceptionConfiguration = a})

-- | The app client ID. If @ClientId@ is null, then the risk configuration is mapped to @userPoolId@ . When the client ID is null, the same risk configuration is applied to all the clients in the userPool. Otherwise, @ClientId@ is mapped to the client. When the client ID is not null, the user pool configuration is overridden and the risk configuration for the client is used instead.
srcClientId :: Lens' SetRiskConfiguration (Maybe Text)
srcClientId = lens _srcClientId (\ s a -> s{_srcClientId = a}) . mapping _Sensitive

-- | The account takeover risk configuration.
srcAccountTakeoverRiskConfiguration :: Lens' SetRiskConfiguration (Maybe AccountTakeoverRiskConfigurationType)
srcAccountTakeoverRiskConfiguration = lens _srcAccountTakeoverRiskConfiguration (\ s a -> s{_srcAccountTakeoverRiskConfiguration = a})

-- | The compromised credentials risk configuration.
srcCompromisedCredentialsRiskConfiguration :: Lens' SetRiskConfiguration (Maybe CompromisedCredentialsRiskConfigurationType)
srcCompromisedCredentialsRiskConfiguration = lens _srcCompromisedCredentialsRiskConfiguration (\ s a -> s{_srcCompromisedCredentialsRiskConfiguration = a})

-- | The user pool ID.
srcUserPoolId :: Lens' SetRiskConfiguration Text
srcUserPoolId = lens _srcUserPoolId (\ s a -> s{_srcUserPoolId = a})

instance AWSRequest SetRiskConfiguration where
        type Rs SetRiskConfiguration =
             SetRiskConfigurationResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 SetRiskConfigurationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "RiskConfiguration"))

instance Hashable SetRiskConfiguration where

instance NFData SetRiskConfiguration where

instance ToHeaders SetRiskConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.SetRiskConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetRiskConfiguration where
        toJSON SetRiskConfiguration'{..}
          = object
              (catMaybes
                 [("RiskExceptionConfiguration" .=) <$>
                    _srcRiskExceptionConfiguration,
                  ("ClientId" .=) <$> _srcClientId,
                  ("AccountTakeoverRiskConfiguration" .=) <$>
                    _srcAccountTakeoverRiskConfiguration,
                  ("CompromisedCredentialsRiskConfiguration" .=) <$>
                    _srcCompromisedCredentialsRiskConfiguration,
                  Just ("UserPoolId" .= _srcUserPoolId)])

instance ToPath SetRiskConfiguration where
        toPath = const "/"

instance ToQuery SetRiskConfiguration where
        toQuery = const mempty

-- | /See:/ 'setRiskConfigurationResponse' smart constructor.
data SetRiskConfigurationResponse = SetRiskConfigurationResponse'
  { _srcrsResponseStatus    :: !Int
  , _srcrsRiskConfiguration :: !RiskConfigurationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetRiskConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcrsResponseStatus' - -- | The response status code.
--
-- * 'srcrsRiskConfiguration' - The risk configuration.
setRiskConfigurationResponse
    :: Int -- ^ 'srcrsResponseStatus'
    -> RiskConfigurationType -- ^ 'srcrsRiskConfiguration'
    -> SetRiskConfigurationResponse
setRiskConfigurationResponse pResponseStatus_ pRiskConfiguration_ =
  SetRiskConfigurationResponse'
    { _srcrsResponseStatus = pResponseStatus_
    , _srcrsRiskConfiguration = pRiskConfiguration_
    }


-- | -- | The response status code.
srcrsResponseStatus :: Lens' SetRiskConfigurationResponse Int
srcrsResponseStatus = lens _srcrsResponseStatus (\ s a -> s{_srcrsResponseStatus = a})

-- | The risk configuration.
srcrsRiskConfiguration :: Lens' SetRiskConfigurationResponse RiskConfigurationType
srcrsRiskConfiguration = lens _srcrsRiskConfiguration (\ s a -> s{_srcrsRiskConfiguration = a})

instance NFData SetRiskConfigurationResponse where
