{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InstanceInformationFilterKey

-- | Describes a filter for a specific list of instances. You can filter instances information by using tags. You specify tags by using a key-value mapping.
--
--
-- Use this action instead of the 'DescribeInstanceInformationRequest$InstanceInformationFilterList' method. The @InstanceInformationFilterList@ method is a legacy method and does not support tags.
--
--
-- /See:/ 'instanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { _iifKey ::
      !InstanceInformationFilterKey,
    _iifValueSet :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceInformationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iifKey' - The name of the filter.
--
-- * 'iifValueSet' - The filter values.
instanceInformationFilter ::
  -- | 'iifKey'
  InstanceInformationFilterKey ->
  -- | 'iifValueSet'
  NonEmpty Text ->
  InstanceInformationFilter
instanceInformationFilter pKey_ pValueSet_ =
  InstanceInformationFilter'
    { _iifKey = pKey_,
      _iifValueSet = _List1 # pValueSet_
    }

-- | The name of the filter.
iifKey :: Lens' InstanceInformationFilter InstanceInformationFilterKey
iifKey = lens _iifKey (\s a -> s {_iifKey = a})

-- | The filter values.
iifValueSet :: Lens' InstanceInformationFilter (NonEmpty Text)
iifValueSet = lens _iifValueSet (\s a -> s {_iifValueSet = a}) . _List1

instance Hashable InstanceInformationFilter

instance NFData InstanceInformationFilter

instance ToJSON InstanceInformationFilter where
  toJSON InstanceInformationFilter' {..} =
    object
      ( catMaybes
          [Just ("key" .= _iifKey), Just ("valueSet" .= _iifValueSet)]
      )
