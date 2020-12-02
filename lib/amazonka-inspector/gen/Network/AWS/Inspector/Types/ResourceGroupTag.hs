{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ResourceGroupTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroupTag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as one of the elements of the 'ResourceGroup' data type.
--
--
--
-- /See:/ 'resourceGroupTag' smart constructor.
data ResourceGroupTag = ResourceGroupTag'
  { _rgtValue ::
      !(Maybe Text),
    _rgtKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceGroupTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgtValue' - The value assigned to a tag key.
--
-- * 'rgtKey' - A tag key.
resourceGroupTag ::
  -- | 'rgtKey'
  Text ->
  ResourceGroupTag
resourceGroupTag pKey_ =
  ResourceGroupTag' {_rgtValue = Nothing, _rgtKey = pKey_}

-- | The value assigned to a tag key.
rgtValue :: Lens' ResourceGroupTag (Maybe Text)
rgtValue = lens _rgtValue (\s a -> s {_rgtValue = a})

-- | A tag key.
rgtKey :: Lens' ResourceGroupTag Text
rgtKey = lens _rgtKey (\s a -> s {_rgtKey = a})

instance FromJSON ResourceGroupTag where
  parseJSON =
    withObject
      "ResourceGroupTag"
      (\x -> ResourceGroupTag' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable ResourceGroupTag

instance NFData ResourceGroupTag

instance ToJSON ResourceGroupTag where
  toJSON ResourceGroupTag' {..} =
    object
      (catMaybes [("value" .=) <$> _rgtValue, Just ("key" .= _rgtKey)])
