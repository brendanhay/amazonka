{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WafAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafAction where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.WafActionType

-- | For the action that is associated with a rule in a @WebACL@ , specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a @WebACL@ , specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a @WebACL@ .
--
--
--
-- /See:/ 'wafAction' smart constructor.
newtype WafAction = WafAction' {_waType :: WafActionType}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WafAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waType' - Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:     * @ALLOW@ : AWS WAF allows requests     * @BLOCK@ : AWS WAF blocks requests     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
wafAction ::
  -- | 'waType'
  WafActionType ->
  WafAction
wafAction pType_ = WafAction' {_waType = pType_}

-- | Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:     * @ALLOW@ : AWS WAF allows requests     * @BLOCK@ : AWS WAF blocks requests     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
waType :: Lens' WafAction WafActionType
waType = lens _waType (\s a -> s {_waType = a})

instance FromJSON WafAction where
  parseJSON =
    withObject "WafAction" (\x -> WafAction' <$> (x .: "Type"))

instance Hashable WafAction

instance NFData WafAction

instance ToJSON WafAction where
  toJSON WafAction' {..} =
    object (catMaybes [Just ("Type" .= _waType)])
