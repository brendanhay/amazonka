{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.IPRuleItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.IPRuleItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a rule for an IP access control group.
--
--
--
-- /See:/ 'ipRuleItem' smart constructor.
data IPRuleItem = IPRuleItem'
  { _iriRuleDesc :: !(Maybe Text),
    _iriIpRule :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPRuleItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iriRuleDesc' - The description.
--
-- * 'iriIpRule' - The IP address range, in CIDR notation.
ipRuleItem ::
  IPRuleItem
ipRuleItem =
  IPRuleItem' {_iriRuleDesc = Nothing, _iriIpRule = Nothing}

-- | The description.
iriRuleDesc :: Lens' IPRuleItem (Maybe Text)
iriRuleDesc = lens _iriRuleDesc (\s a -> s {_iriRuleDesc = a})

-- | The IP address range, in CIDR notation.
iriIpRule :: Lens' IPRuleItem (Maybe Text)
iriIpRule = lens _iriIpRule (\s a -> s {_iriIpRule = a})

instance FromJSON IPRuleItem where
  parseJSON =
    withObject
      "IPRuleItem"
      (\x -> IPRuleItem' <$> (x .:? "ruleDesc") <*> (x .:? "ipRule"))

instance Hashable IPRuleItem

instance NFData IPRuleItem

instance ToJSON IPRuleItem where
  toJSON IPRuleItem' {..} =
    object
      ( catMaybes
          [("ruleDesc" .=) <$> _iriRuleDesc, ("ipRule" .=) <$> _iriIpRule]
      )
