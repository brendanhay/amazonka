{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Sort
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Sort where

import Network.AWS.AlexaBusiness.Types.SortValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a sort criteria.
--
--
--
-- /See:/ 'sort' smart constructor.
data Sort = Sort' {_sKey :: !Text, _sValue :: !SortValue}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Sort' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sKey' - The sort key of a sort object.
--
-- * 'sValue' - The sort value of a sort object.
sort ::
  -- | 'sKey'
  Text ->
  -- | 'sValue'
  SortValue ->
  Sort
sort pKey_ pValue_ = Sort' {_sKey = pKey_, _sValue = pValue_}

-- | The sort key of a sort object.
sKey :: Lens' Sort Text
sKey = lens _sKey (\s a -> s {_sKey = a})

-- | The sort value of a sort object.
sValue :: Lens' Sort SortValue
sValue = lens _sValue (\s a -> s {_sValue = a})

instance Hashable Sort

instance NFData Sort

instance ToJSON Sort where
  toJSON Sort' {..} =
    object
      (catMaybes [Just ("Key" .= _sKey), Just ("Value" .= _sValue)])
