{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Network.AWS.CognitoIdentityProvider.Types.EventFilterType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The compromised credentials risk configuration type.
--
--
--
-- /See:/ 'compromisedCredentialsRiskConfigurationType' smart constructor.
data CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType'
  { _ccrctEventFilter ::
      !( Maybe
           [EventFilterType]
       ),
    _ccrctActions ::
      !CompromisedCredentialsActionsType
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CompromisedCredentialsRiskConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrctEventFilter' - Perform the action for these events. The default is to perform all events if no event filter is specified.
--
-- * 'ccrctActions' - The compromised credentials risk configuration actions.
compromisedCredentialsRiskConfigurationType ::
  -- | 'ccrctActions'
  CompromisedCredentialsActionsType ->
  CompromisedCredentialsRiskConfigurationType
compromisedCredentialsRiskConfigurationType pActions_ =
  CompromisedCredentialsRiskConfigurationType'
    { _ccrctEventFilter =
        Nothing,
      _ccrctActions = pActions_
    }

-- | Perform the action for these events. The default is to perform all events if no event filter is specified.
ccrctEventFilter :: Lens' CompromisedCredentialsRiskConfigurationType [EventFilterType]
ccrctEventFilter = lens _ccrctEventFilter (\s a -> s {_ccrctEventFilter = a}) . _Default . _Coerce

-- | The compromised credentials risk configuration actions.
ccrctActions :: Lens' CompromisedCredentialsRiskConfigurationType CompromisedCredentialsActionsType
ccrctActions = lens _ccrctActions (\s a -> s {_ccrctActions = a})

instance FromJSON CompromisedCredentialsRiskConfigurationType where
  parseJSON =
    withObject
      "CompromisedCredentialsRiskConfigurationType"
      ( \x ->
          CompromisedCredentialsRiskConfigurationType'
            <$> (x .:? "EventFilter" .!= mempty) <*> (x .: "Actions")
      )

instance Hashable CompromisedCredentialsRiskConfigurationType

instance NFData CompromisedCredentialsRiskConfigurationType

instance ToJSON CompromisedCredentialsRiskConfigurationType where
  toJSON CompromisedCredentialsRiskConfigurationType' {..} =
    object
      ( catMaybes
          [ ("EventFilter" .=) <$> _ccrctEventFilter,
            Just ("Actions" .= _ccrctActions)
          ]
      )
