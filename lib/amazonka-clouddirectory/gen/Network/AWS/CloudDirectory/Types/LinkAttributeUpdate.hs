{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.LinkAttributeUpdate where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.LinkAttributeAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains attribute update information.
--
--
--
-- /See:/ 'linkAttributeUpdate' smart constructor.
data LinkAttributeUpdate = LinkAttributeUpdate'
  { _lauAttributeAction ::
      !(Maybe LinkAttributeAction),
    _lauAttributeKey :: !(Maybe AttributeKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LinkAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lauAttributeAction' - The action to perform as part of the attribute update.
--
-- * 'lauAttributeKey' - The key of the attribute being updated.
linkAttributeUpdate ::
  LinkAttributeUpdate
linkAttributeUpdate =
  LinkAttributeUpdate'
    { _lauAttributeAction = Nothing,
      _lauAttributeKey = Nothing
    }

-- | The action to perform as part of the attribute update.
lauAttributeAction :: Lens' LinkAttributeUpdate (Maybe LinkAttributeAction)
lauAttributeAction = lens _lauAttributeAction (\s a -> s {_lauAttributeAction = a})

-- | The key of the attribute being updated.
lauAttributeKey :: Lens' LinkAttributeUpdate (Maybe AttributeKey)
lauAttributeKey = lens _lauAttributeKey (\s a -> s {_lauAttributeKey = a})

instance Hashable LinkAttributeUpdate

instance NFData LinkAttributeUpdate

instance ToJSON LinkAttributeUpdate where
  toJSON LinkAttributeUpdate' {..} =
    object
      ( catMaybes
          [ ("AttributeAction" .=) <$> _lauAttributeAction,
            ("AttributeKey" .=) <$> _lauAttributeKey
          ]
      )
