{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ExcludedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ExcludedRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The rule to exclude from a rule group. This is applicable only when the @ActivatedRule@ refers to a @RuleGroup@ . The rule must belong to the @RuleGroup@ that is specified by the @ActivatedRule@ .
--
--
--
-- /See:/ 'excludedRule' smart constructor.
newtype ExcludedRule = ExcludedRule' {_erRuleId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExcludedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erRuleId' - The unique identifier for the rule to exclude from the rule group.
excludedRule ::
  -- | 'erRuleId'
  Text ->
  ExcludedRule
excludedRule pRuleId_ = ExcludedRule' {_erRuleId = pRuleId_}

-- | The unique identifier for the rule to exclude from the rule group.
erRuleId :: Lens' ExcludedRule Text
erRuleId = lens _erRuleId (\s a -> s {_erRuleId = a})

instance FromJSON ExcludedRule where
  parseJSON =
    withObject
      "ExcludedRule"
      (\x -> ExcludedRule' <$> (x .: "RuleId"))

instance Hashable ExcludedRule

instance NFData ExcludedRule

instance ToJSON ExcludedRule where
  toJSON ExcludedRule' {..} =
    object (catMaybes [Just ("RuleId" .= _erRuleId)])
