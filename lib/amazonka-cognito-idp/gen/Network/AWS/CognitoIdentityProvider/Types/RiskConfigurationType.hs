{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The risk configuration type.
--
--
--
-- /See:/ 'riskConfigurationType' smart constructor.
data RiskConfigurationType = RiskConfigurationType'
  { _rctRiskExceptionConfiguration ::
      !(Maybe RiskExceptionConfigurationType),
    _rctClientId :: !(Maybe (Sensitive Text)),
    _rctAccountTakeoverRiskConfiguration ::
      !(Maybe AccountTakeoverRiskConfigurationType),
    _rctLastModifiedDate :: !(Maybe POSIX),
    _rctUserPoolId :: !(Maybe Text),
    _rctCompromisedCredentialsRiskConfiguration ::
      !( Maybe
           CompromisedCredentialsRiskConfigurationType
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'RiskConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rctRiskExceptionConfiguration' - The configuration to override the risk decision.
--
-- * 'rctClientId' - The app client ID.
--
-- * 'rctAccountTakeoverRiskConfiguration' - The account takeover risk configuration object including the @NotifyConfiguration@ object and @Actions@ to take in the case of an account takeover.
--
-- * 'rctLastModifiedDate' - The last modified date.
--
-- * 'rctUserPoolId' - The user pool ID.
--
-- * 'rctCompromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration object including the @EventFilter@ and the @EventAction@
riskConfigurationType ::
  RiskConfigurationType
riskConfigurationType =
  RiskConfigurationType'
    { _rctRiskExceptionConfiguration = Nothing,
      _rctClientId = Nothing,
      _rctAccountTakeoverRiskConfiguration = Nothing,
      _rctLastModifiedDate = Nothing,
      _rctUserPoolId = Nothing,
      _rctCompromisedCredentialsRiskConfiguration = Nothing
    }

-- | The configuration to override the risk decision.
rctRiskExceptionConfiguration :: Lens' RiskConfigurationType (Maybe RiskExceptionConfigurationType)
rctRiskExceptionConfiguration = lens _rctRiskExceptionConfiguration (\s a -> s {_rctRiskExceptionConfiguration = a})

-- | The app client ID.
rctClientId :: Lens' RiskConfigurationType (Maybe Text)
rctClientId = lens _rctClientId (\s a -> s {_rctClientId = a}) . mapping _Sensitive

-- | The account takeover risk configuration object including the @NotifyConfiguration@ object and @Actions@ to take in the case of an account takeover.
rctAccountTakeoverRiskConfiguration :: Lens' RiskConfigurationType (Maybe AccountTakeoverRiskConfigurationType)
rctAccountTakeoverRiskConfiguration = lens _rctAccountTakeoverRiskConfiguration (\s a -> s {_rctAccountTakeoverRiskConfiguration = a})

-- | The last modified date.
rctLastModifiedDate :: Lens' RiskConfigurationType (Maybe UTCTime)
rctLastModifiedDate = lens _rctLastModifiedDate (\s a -> s {_rctLastModifiedDate = a}) . mapping _Time

-- | The user pool ID.
rctUserPoolId :: Lens' RiskConfigurationType (Maybe Text)
rctUserPoolId = lens _rctUserPoolId (\s a -> s {_rctUserPoolId = a})

-- | The compromised credentials risk configuration object including the @EventFilter@ and the @EventAction@
rctCompromisedCredentialsRiskConfiguration :: Lens' RiskConfigurationType (Maybe CompromisedCredentialsRiskConfigurationType)
rctCompromisedCredentialsRiskConfiguration = lens _rctCompromisedCredentialsRiskConfiguration (\s a -> s {_rctCompromisedCredentialsRiskConfiguration = a})

instance FromJSON RiskConfigurationType where
  parseJSON =
    withObject
      "RiskConfigurationType"
      ( \x ->
          RiskConfigurationType'
            <$> (x .:? "RiskExceptionConfiguration")
            <*> (x .:? "ClientId")
            <*> (x .:? "AccountTakeoverRiskConfiguration")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "CompromisedCredentialsRiskConfiguration")
      )

instance Hashable RiskConfigurationType

instance NFData RiskConfigurationType
