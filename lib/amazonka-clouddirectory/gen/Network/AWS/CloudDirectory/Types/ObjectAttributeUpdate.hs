{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains attribute update information.
--
--
--
-- /See:/ 'objectAttributeUpdate' smart constructor.
data ObjectAttributeUpdate = ObjectAttributeUpdate'
  { _oauObjectAttributeAction ::
      !(Maybe ObjectAttributeAction),
    _oauObjectAttributeKey :: !(Maybe AttributeKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oauObjectAttributeAction' - The action to perform as part of the attribute update.
--
-- * 'oauObjectAttributeKey' - The key of the attribute being updated.
objectAttributeUpdate ::
  ObjectAttributeUpdate
objectAttributeUpdate =
  ObjectAttributeUpdate'
    { _oauObjectAttributeAction = Nothing,
      _oauObjectAttributeKey = Nothing
    }

-- | The action to perform as part of the attribute update.
oauObjectAttributeAction :: Lens' ObjectAttributeUpdate (Maybe ObjectAttributeAction)
oauObjectAttributeAction = lens _oauObjectAttributeAction (\s a -> s {_oauObjectAttributeAction = a})

-- | The key of the attribute being updated.
oauObjectAttributeKey :: Lens' ObjectAttributeUpdate (Maybe AttributeKey)
oauObjectAttributeKey = lens _oauObjectAttributeKey (\s a -> s {_oauObjectAttributeKey = a})

instance Hashable ObjectAttributeUpdate

instance NFData ObjectAttributeUpdate

instance ToJSON ObjectAttributeUpdate where
  toJSON ObjectAttributeUpdate' {..} =
    object
      ( catMaybes
          [ ("ObjectAttributeAction" .=) <$> _oauObjectAttributeAction,
            ("ObjectAttributeKey" .=) <$> _oauObjectAttributeKey
          ]
      )
