{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ListenerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ListenerDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Listener
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The policies enabled for a listener.
--
--
--
-- /See:/ 'listenerDescription' smart constructor.
data ListenerDescription = ListenerDescription'
  { _ldPolicyNames ::
      !(Maybe [Text]),
    _ldListener :: !(Maybe Listener)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListenerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldPolicyNames' - The policies. If there are no policies enabled, the list is empty.
--
-- * 'ldListener' - The listener.
listenerDescription ::
  ListenerDescription
listenerDescription =
  ListenerDescription'
    { _ldPolicyNames = Nothing,
      _ldListener = Nothing
    }

-- | The policies. If there are no policies enabled, the list is empty.
ldPolicyNames :: Lens' ListenerDescription [Text]
ldPolicyNames = lens _ldPolicyNames (\s a -> s {_ldPolicyNames = a}) . _Default . _Coerce

-- | The listener.
ldListener :: Lens' ListenerDescription (Maybe Listener)
ldListener = lens _ldListener (\s a -> s {_ldListener = a})

instance FromXML ListenerDescription where
  parseXML x =
    ListenerDescription'
      <$> (x .@? "PolicyNames" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Listener")

instance Hashable ListenerDescription

instance NFData ListenerDescription
