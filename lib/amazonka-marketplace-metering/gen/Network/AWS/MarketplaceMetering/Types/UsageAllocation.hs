{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageAllocation where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types.Tag
import Network.AWS.Prelude

-- | Usage allocations allow you to split usage into buckets by tags.
--
--
-- Each UsageAllocation indicates the usage quantity for a specific set of tags.
--
--
-- /See:/ 'usageAllocation' smart constructor.
data UsageAllocation = UsageAllocation'
  { _uaTags ::
      !(Maybe (List1 Tag)),
    _uaAllocatedUsageQuantity :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaTags' - The set of tags that define the bucket of usage. For the bucket of items with no tags, this parameter can be left out.
--
-- * 'uaAllocatedUsageQuantity' - The total quantity allocated to this bucket of usage.
usageAllocation ::
  -- | 'uaAllocatedUsageQuantity'
  Natural ->
  UsageAllocation
usageAllocation pAllocatedUsageQuantity_ =
  UsageAllocation'
    { _uaTags = Nothing,
      _uaAllocatedUsageQuantity = _Nat # pAllocatedUsageQuantity_
    }

-- | The set of tags that define the bucket of usage. For the bucket of items with no tags, this parameter can be left out.
uaTags :: Lens' UsageAllocation (Maybe (NonEmpty Tag))
uaTags = lens _uaTags (\s a -> s {_uaTags = a}) . mapping _List1

-- | The total quantity allocated to this bucket of usage.
uaAllocatedUsageQuantity :: Lens' UsageAllocation Natural
uaAllocatedUsageQuantity = lens _uaAllocatedUsageQuantity (\s a -> s {_uaAllocatedUsageQuantity = a}) . _Nat

instance FromJSON UsageAllocation where
  parseJSON =
    withObject
      "UsageAllocation"
      ( \x ->
          UsageAllocation'
            <$> (x .:? "Tags") <*> (x .: "AllocatedUsageQuantity")
      )

instance Hashable UsageAllocation

instance NFData UsageAllocation

instance ToJSON UsageAllocation where
  toJSON UsageAllocation' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _uaTags,
            Just ("AllocatedUsageQuantity" .= _uaAllocatedUsageQuantity)
          ]
      )
