{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOnRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOnRequest where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AddOnType
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import Network.AWS.Prelude

-- | Describes a request to enable, modify, or disable an add-on for an Amazon Lightsail resource.
--
--
--
-- /See:/ 'addOnRequest' smart constructor.
data AddOnRequest = AddOnRequest'
  { _aorAutoSnapshotAddOnRequest ::
      !(Maybe AutoSnapshotAddOnRequest),
    _aorAddOnType :: !AddOnType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddOnRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aorAutoSnapshotAddOnRequest' - An object that represents additional parameters when enabling or modifying the automatic snapshot add-on.
--
-- * 'aorAddOnType' - The add-on type.
addOnRequest ::
  -- | 'aorAddOnType'
  AddOnType ->
  AddOnRequest
addOnRequest pAddOnType_ =
  AddOnRequest'
    { _aorAutoSnapshotAddOnRequest = Nothing,
      _aorAddOnType = pAddOnType_
    }

-- | An object that represents additional parameters when enabling or modifying the automatic snapshot add-on.
aorAutoSnapshotAddOnRequest :: Lens' AddOnRequest (Maybe AutoSnapshotAddOnRequest)
aorAutoSnapshotAddOnRequest = lens _aorAutoSnapshotAddOnRequest (\s a -> s {_aorAutoSnapshotAddOnRequest = a})

-- | The add-on type.
aorAddOnType :: Lens' AddOnRequest AddOnType
aorAddOnType = lens _aorAddOnType (\s a -> s {_aorAddOnType = a})

instance Hashable AddOnRequest

instance NFData AddOnRequest

instance ToJSON AddOnRequest where
  toJSON AddOnRequest' {..} =
    object
      ( catMaybes
          [ ("autoSnapshotAddOnRequest" .=) <$> _aorAutoSnapshotAddOnRequest,
            Just ("addOnType" .= _aorAddOnType)
          ]
      )
