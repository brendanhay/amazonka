{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType where

import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The compromised credentials actions type
--
--
--
-- /See:/ 'compromisedCredentialsActionsType' smart constructor.
newtype CompromisedCredentialsActionsType = CompromisedCredentialsActionsType'
  { _ccatEventAction ::
      CompromisedCredentialsEventActionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompromisedCredentialsActionsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccatEventAction' - The event action.
compromisedCredentialsActionsType ::
  -- | 'ccatEventAction'
  CompromisedCredentialsEventActionType ->
  CompromisedCredentialsActionsType
compromisedCredentialsActionsType pEventAction_ =
  CompromisedCredentialsActionsType'
    { _ccatEventAction =
        pEventAction_
    }

-- | The event action.
ccatEventAction :: Lens' CompromisedCredentialsActionsType CompromisedCredentialsEventActionType
ccatEventAction = lens _ccatEventAction (\s a -> s {_ccatEventAction = a})

instance FromJSON CompromisedCredentialsActionsType where
  parseJSON =
    withObject
      "CompromisedCredentialsActionsType"
      ( \x ->
          CompromisedCredentialsActionsType' <$> (x .: "EventAction")
      )

instance Hashable CompromisedCredentialsActionsType

instance NFData CompromisedCredentialsActionsType

instance ToJSON CompromisedCredentialsActionsType where
  toJSON CompromisedCredentialsActionsType' {..} =
    object (catMaybes [Just ("EventAction" .= _ccatEventAction)])
