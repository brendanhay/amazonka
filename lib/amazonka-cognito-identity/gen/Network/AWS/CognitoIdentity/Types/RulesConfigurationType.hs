{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RulesConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RulesConfigurationType where

import Network.AWS.CognitoIdentity.Types.MappingRule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for rules.
--
--
--
-- /See:/ 'rulesConfigurationType' smart constructor.
newtype RulesConfigurationType = RulesConfigurationType'
  { _rctRules ::
      List1 MappingRule
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RulesConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rctRules' - An array of rules. You can specify up to 25 rules per identity provider. Rules are evaluated in order. The first one to match specifies the role.
rulesConfigurationType ::
  -- | 'rctRules'
  NonEmpty MappingRule ->
  RulesConfigurationType
rulesConfigurationType pRules_ =
  RulesConfigurationType' {_rctRules = _List1 # pRules_}

-- | An array of rules. You can specify up to 25 rules per identity provider. Rules are evaluated in order. The first one to match specifies the role.
rctRules :: Lens' RulesConfigurationType (NonEmpty MappingRule)
rctRules = lens _rctRules (\s a -> s {_rctRules = a}) . _List1

instance FromJSON RulesConfigurationType where
  parseJSON =
    withObject
      "RulesConfigurationType"
      (\x -> RulesConfigurationType' <$> (x .: "Rules"))

instance Hashable RulesConfigurationType

instance NFData RulesConfigurationType

instance ToJSON RulesConfigurationType where
  toJSON RulesConfigurationType' {..} =
    object (catMaybes [Just ("Rules" .= _rctRules)])
