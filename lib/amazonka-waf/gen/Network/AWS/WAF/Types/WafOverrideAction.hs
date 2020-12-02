{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WafOverrideAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WafOverrideAction where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.WafOverrideActionType

-- | The action to take if any rule within the @RuleGroup@ matches a request.
--
--
--
-- /See:/ 'wafOverrideAction' smart constructor.
newtype WafOverrideAction = WafOverrideAction'
  { _woaType ::
      WafOverrideActionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WafOverrideAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'woaType' - @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
wafOverrideAction ::
  -- | 'woaType'
  WafOverrideActionType ->
  WafOverrideAction
wafOverrideAction pType_ = WafOverrideAction' {_woaType = pType_}

-- | @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
woaType :: Lens' WafOverrideAction WafOverrideActionType
woaType = lens _woaType (\s a -> s {_woaType = a})

instance FromJSON WafOverrideAction where
  parseJSON =
    withObject
      "WafOverrideAction"
      (\x -> WafOverrideAction' <$> (x .: "Type"))

instance Hashable WafOverrideAction

instance NFData WafOverrideAction

instance ToJSON WafOverrideAction where
  toJSON WafOverrideAction' {..} =
    object (catMaybes [Just ("Type" .= _woaType)])
