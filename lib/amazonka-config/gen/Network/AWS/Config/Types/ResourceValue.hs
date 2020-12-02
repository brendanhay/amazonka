{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceValue where

import Network.AWS.Config.Types.ResourceValueType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The dynamic value of the resource.
--
--
--
-- /See:/ 'resourceValue' smart constructor.
newtype ResourceValue = ResourceValue'
  { _rvValue ::
      ResourceValueType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvValue' - The value is a resource ID.
resourceValue ::
  -- | 'rvValue'
  ResourceValueType ->
  ResourceValue
resourceValue pValue_ = ResourceValue' {_rvValue = pValue_}

-- | The value is a resource ID.
rvValue :: Lens' ResourceValue ResourceValueType
rvValue = lens _rvValue (\s a -> s {_rvValue = a})

instance FromJSON ResourceValue where
  parseJSON =
    withObject
      "ResourceValue"
      (\x -> ResourceValue' <$> (x .: "Value"))

instance Hashable ResourceValue

instance NFData ResourceValue

instance ToJSON ResourceValue where
  toJSON ResourceValue' {..} =
    object (catMaybes [Just ("Value" .= _rvValue)])
