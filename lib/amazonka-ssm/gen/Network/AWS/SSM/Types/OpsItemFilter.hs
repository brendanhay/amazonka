{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OpsItemFilterKey
import Network.AWS.SSM.Types.OpsItemFilterOperator

-- | Describes an OpsItem filter.
--
--
--
-- /See:/ 'opsItemFilter' smart constructor.
data OpsItemFilter = OpsItemFilter'
  { _oifKey :: !OpsItemFilterKey,
    _oifValues :: ![Text],
    _oifOperator :: !OpsItemFilterOperator
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsItemFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oifKey' - The name of the filter.
--
-- * 'oifValues' - The filter value.
--
-- * 'oifOperator' - The operator used by the filter call.
opsItemFilter ::
  -- | 'oifKey'
  OpsItemFilterKey ->
  -- | 'oifOperator'
  OpsItemFilterOperator ->
  OpsItemFilter
opsItemFilter pKey_ pOperator_ =
  OpsItemFilter'
    { _oifKey = pKey_,
      _oifValues = mempty,
      _oifOperator = pOperator_
    }

-- | The name of the filter.
oifKey :: Lens' OpsItemFilter OpsItemFilterKey
oifKey = lens _oifKey (\s a -> s {_oifKey = a})

-- | The filter value.
oifValues :: Lens' OpsItemFilter [Text]
oifValues = lens _oifValues (\s a -> s {_oifValues = a}) . _Coerce

-- | The operator used by the filter call.
oifOperator :: Lens' OpsItemFilter OpsItemFilterOperator
oifOperator = lens _oifOperator (\s a -> s {_oifOperator = a})

instance Hashable OpsItemFilter

instance NFData OpsItemFilter

instance ToJSON OpsItemFilter where
  toJSON OpsItemFilter' {..} =
    object
      ( catMaybes
          [ Just ("Key" .= _oifKey),
            Just ("Values" .= _oifValues),
            Just ("Operator" .= _oifOperator)
          ]
      )
