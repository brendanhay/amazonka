{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResultAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResultAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The inventory item result attribute.
--
--
--
-- /See:/ 'resultAttribute' smart constructor.
newtype ResultAttribute = ResultAttribute' {_raTypeName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raTypeName' - Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
resultAttribute ::
  -- | 'raTypeName'
  Text ->
  ResultAttribute
resultAttribute pTypeName_ =
  ResultAttribute' {_raTypeName = pTypeName_}

-- | Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
raTypeName :: Lens' ResultAttribute Text
raTypeName = lens _raTypeName (\s a -> s {_raTypeName = a})

instance Hashable ResultAttribute

instance NFData ResultAttribute

instance ToJSON ResultAttribute where
  toJSON ResultAttribute' {..} =
    object (catMaybes [Just ("TypeName" .= _raTypeName)])
