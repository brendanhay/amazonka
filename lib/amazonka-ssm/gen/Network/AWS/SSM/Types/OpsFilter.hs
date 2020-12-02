{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OpsFilterOperatorType

-- | A filter for viewing OpsItem summaries.
--
--
--
-- /See:/ 'opsFilter' smart constructor.
data OpsFilter = OpsFilter'
  { _ofType ::
      !(Maybe OpsFilterOperatorType),
    _ofKey :: !Text,
    _ofValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofType' - The type of filter.
--
-- * 'ofKey' - The name of the filter.
--
-- * 'ofValues' - The filter value.
opsFilter ::
  -- | 'ofKey'
  Text ->
  -- | 'ofValues'
  NonEmpty Text ->
  OpsFilter
opsFilter pKey_ pValues_ =
  OpsFilter'
    { _ofType = Nothing,
      _ofKey = pKey_,
      _ofValues = _List1 # pValues_
    }

-- | The type of filter.
ofType :: Lens' OpsFilter (Maybe OpsFilterOperatorType)
ofType = lens _ofType (\s a -> s {_ofType = a})

-- | The name of the filter.
ofKey :: Lens' OpsFilter Text
ofKey = lens _ofKey (\s a -> s {_ofKey = a})

-- | The filter value.
ofValues :: Lens' OpsFilter (NonEmpty Text)
ofValues = lens _ofValues (\s a -> s {_ofValues = a}) . _List1

instance Hashable OpsFilter

instance NFData OpsFilter

instance ToJSON OpsFilter where
  toJSON OpsFilter' {..} =
    object
      ( catMaybes
          [ ("Type" .=) <$> _ofType,
            Just ("Key" .= _ofKey),
            Just ("Values" .= _ofValues)
          ]
      )
