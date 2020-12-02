{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType where

import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A map containing a priority as a key, and recovery method name as a value.
--
--
--
-- /See:/ 'recoveryOptionType' smart constructor.
data RecoveryOptionType = RecoveryOptionType'
  { _rotPriority :: !Nat,
    _rotName :: !RecoveryOptionNameType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecoveryOptionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rotPriority' - A positive integer specifying priority of a method with 1 being the highest priority.
--
-- * 'rotName' - Specifies the recovery method for a user.
recoveryOptionType ::
  -- | 'rotPriority'
  Natural ->
  -- | 'rotName'
  RecoveryOptionNameType ->
  RecoveryOptionType
recoveryOptionType pPriority_ pName_ =
  RecoveryOptionType'
    { _rotPriority = _Nat # pPriority_,
      _rotName = pName_
    }

-- | A positive integer specifying priority of a method with 1 being the highest priority.
rotPriority :: Lens' RecoveryOptionType Natural
rotPriority = lens _rotPriority (\s a -> s {_rotPriority = a}) . _Nat

-- | Specifies the recovery method for a user.
rotName :: Lens' RecoveryOptionType RecoveryOptionNameType
rotName = lens _rotName (\s a -> s {_rotName = a})

instance FromJSON RecoveryOptionType where
  parseJSON =
    withObject
      "RecoveryOptionType"
      ( \x ->
          RecoveryOptionType' <$> (x .: "Priority") <*> (x .: "Name")
      )

instance Hashable RecoveryOptionType

instance NFData RecoveryOptionType

instance ToJSON RecoveryOptionType where
  toJSON RecoveryOptionType' {..} =
    object
      ( catMaybes
          [Just ("Priority" .= _rotPriority), Just ("Name" .= _rotName)]
      )
