{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParametersFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParametersFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ParametersFilterKey

-- | This data type is deprecated. Instead, use 'ParameterStringFilter' .
--
--
--
-- /See:/ 'parametersFilter' smart constructor.
data ParametersFilter = ParametersFilter'
  { _pKey ::
      !ParametersFilterKey,
    _pValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParametersFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pKey' - The name of the filter.
--
-- * 'pValues' - The filter values.
parametersFilter ::
  -- | 'pKey'
  ParametersFilterKey ->
  -- | 'pValues'
  NonEmpty Text ->
  ParametersFilter
parametersFilter pKey_ pValues_ =
  ParametersFilter' {_pKey = pKey_, _pValues = _List1 # pValues_}

-- | The name of the filter.
pKey :: Lens' ParametersFilter ParametersFilterKey
pKey = lens _pKey (\s a -> s {_pKey = a})

-- | The filter values.
pValues :: Lens' ParametersFilter (NonEmpty Text)
pValues = lens _pValues (\s a -> s {_pValues = a}) . _List1

instance Hashable ParametersFilter

instance NFData ParametersFilter

instance ToJSON ParametersFilter where
  toJSON ParametersFilter' {..} =
    object
      (catMaybes [Just ("Key" .= _pKey), Just ("Values" .= _pValues)])
