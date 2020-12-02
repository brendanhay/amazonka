{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the identifier and the friendly name or description of the @Rule@ .
--
--
--
-- /See:/ 'ruleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { _rsRuleId :: !Text,
    _rsName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuleSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRuleId' - A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- * 'rsName' - A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
ruleSummary ::
  -- | 'rsRuleId'
  Text ->
  -- | 'rsName'
  Text ->
  RuleSummary
ruleSummary pRuleId_ pName_ =
  RuleSummary' {_rsRuleId = pRuleId_, _rsName = pName_}

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
rsRuleId :: Lens' RuleSummary Text
rsRuleId = lens _rsRuleId (\s a -> s {_rsRuleId = a})

-- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
rsName :: Lens' RuleSummary Text
rsName = lens _rsName (\s a -> s {_rsName = a})

instance FromJSON RuleSummary where
  parseJSON =
    withObject
      "RuleSummary"
      (\x -> RuleSummary' <$> (x .: "RuleId") <*> (x .: "Name"))

instance Hashable RuleSummary

instance NFData RuleSummary
