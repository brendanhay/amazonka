{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchStateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchStateFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InstancePatchStateOperatorType

-- | Defines a filter used in 'DescribeInstancePatchStatesForPatchGroup' used to scope down the information returned by the API.
--
--
--
-- /See:/ 'instancePatchStateFilter' smart constructor.
data InstancePatchStateFilter = InstancePatchStateFilter'
  { _ipsfKey ::
      !Text,
    _ipsfValues :: !(List1 Text),
    _ipsfType ::
      !InstancePatchStateOperatorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancePatchStateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsfKey' - The key for the filter. Supported values are FailedCount, InstalledCount, InstalledOtherCount, MissingCount and NotApplicableCount.
--
-- * 'ipsfValues' - The value for the filter, must be an integer greater than or equal to 0.
--
-- * 'ipsfType' - The type of comparison that should be performed for the value: Equal, NotEqual, LessThan or GreaterThan.
instancePatchStateFilter ::
  -- | 'ipsfKey'
  Text ->
  -- | 'ipsfValues'
  NonEmpty Text ->
  -- | 'ipsfType'
  InstancePatchStateOperatorType ->
  InstancePatchStateFilter
instancePatchStateFilter pKey_ pValues_ pType_ =
  InstancePatchStateFilter'
    { _ipsfKey = pKey_,
      _ipsfValues = _List1 # pValues_,
      _ipsfType = pType_
    }

-- | The key for the filter. Supported values are FailedCount, InstalledCount, InstalledOtherCount, MissingCount and NotApplicableCount.
ipsfKey :: Lens' InstancePatchStateFilter Text
ipsfKey = lens _ipsfKey (\s a -> s {_ipsfKey = a})

-- | The value for the filter, must be an integer greater than or equal to 0.
ipsfValues :: Lens' InstancePatchStateFilter (NonEmpty Text)
ipsfValues = lens _ipsfValues (\s a -> s {_ipsfValues = a}) . _List1

-- | The type of comparison that should be performed for the value: Equal, NotEqual, LessThan or GreaterThan.
ipsfType :: Lens' InstancePatchStateFilter InstancePatchStateOperatorType
ipsfType = lens _ipsfType (\s a -> s {_ipsfType = a})

instance Hashable InstancePatchStateFilter

instance NFData InstancePatchStateFilter

instance ToJSON InstancePatchStateFilter where
  toJSON InstancePatchStateFilter' {..} =
    object
      ( catMaybes
          [ Just ("Key" .= _ipsfKey),
            Just ("Values" .= _ipsfValues),
            Just ("Type" .= _ipsfType)
          ]
      )
