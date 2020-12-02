{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem where

import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the provisioned concurrency configuration for a function alias or version.
--
--
--
-- /See:/ 'provisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { _pccliStatus ::
      !( Maybe
           ProvisionedConcurrencyStatusEnum
       ),
    _pccliFunctionARN ::
      !(Maybe Text),
    _pccliRequestedProvisionedConcurrentExecutions ::
      !(Maybe Nat),
    _pccliAvailableProvisionedConcurrentExecutions ::
      !(Maybe Nat),
    _pccliStatusReason ::
      !(Maybe Text),
    _pccliAllocatedProvisionedConcurrentExecutions ::
      !(Maybe Nat),
    _pccliLastModified ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedConcurrencyConfigListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pccliStatus' - The status of the allocation process.
--
-- * 'pccliFunctionARN' - The Amazon Resource Name (ARN) of the alias or version.
--
-- * 'pccliRequestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- * 'pccliAvailableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- * 'pccliStatusReason' - For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- * 'pccliAllocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
--
-- * 'pccliLastModified' - The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
provisionedConcurrencyConfigListItem ::
  ProvisionedConcurrencyConfigListItem
provisionedConcurrencyConfigListItem =
  ProvisionedConcurrencyConfigListItem'
    { _pccliStatus = Nothing,
      _pccliFunctionARN = Nothing,
      _pccliRequestedProvisionedConcurrentExecutions = Nothing,
      _pccliAvailableProvisionedConcurrentExecutions = Nothing,
      _pccliStatusReason = Nothing,
      _pccliAllocatedProvisionedConcurrentExecutions = Nothing,
      _pccliLastModified = Nothing
    }

-- | The status of the allocation process.
pccliStatus :: Lens' ProvisionedConcurrencyConfigListItem (Maybe ProvisionedConcurrencyStatusEnum)
pccliStatus = lens _pccliStatus (\s a -> s {_pccliStatus = a})

-- | The Amazon Resource Name (ARN) of the alias or version.
pccliFunctionARN :: Lens' ProvisionedConcurrencyConfigListItem (Maybe Text)
pccliFunctionARN = lens _pccliFunctionARN (\s a -> s {_pccliFunctionARN = a})

-- | The amount of provisioned concurrency requested.
pccliRequestedProvisionedConcurrentExecutions :: Lens' ProvisionedConcurrencyConfigListItem (Maybe Natural)
pccliRequestedProvisionedConcurrentExecutions = lens _pccliRequestedProvisionedConcurrentExecutions (\s a -> s {_pccliRequestedProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | The amount of provisioned concurrency available.
pccliAvailableProvisionedConcurrentExecutions :: Lens' ProvisionedConcurrencyConfigListItem (Maybe Natural)
pccliAvailableProvisionedConcurrentExecutions = lens _pccliAvailableProvisionedConcurrentExecutions (\s a -> s {_pccliAvailableProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
pccliStatusReason :: Lens' ProvisionedConcurrencyConfigListItem (Maybe Text)
pccliStatusReason = lens _pccliStatusReason (\s a -> s {_pccliStatusReason = a})

-- | The amount of provisioned concurrency allocated.
pccliAllocatedProvisionedConcurrentExecutions :: Lens' ProvisionedConcurrencyConfigListItem (Maybe Natural)
pccliAllocatedProvisionedConcurrentExecutions = lens _pccliAllocatedProvisionedConcurrentExecutions (\s a -> s {_pccliAllocatedProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
pccliLastModified :: Lens' ProvisionedConcurrencyConfigListItem (Maybe Text)
pccliLastModified = lens _pccliLastModified (\s a -> s {_pccliLastModified = a})

instance FromJSON ProvisionedConcurrencyConfigListItem where
  parseJSON =
    withObject
      "ProvisionedConcurrencyConfigListItem"
      ( \x ->
          ProvisionedConcurrencyConfigListItem'
            <$> (x .:? "Status")
            <*> (x .:? "FunctionArn")
            <*> (x .:? "RequestedProvisionedConcurrentExecutions")
            <*> (x .:? "AvailableProvisionedConcurrentExecutions")
            <*> (x .:? "StatusReason")
            <*> (x .:? "AllocatedProvisionedConcurrentExecutions")
            <*> (x .:? "LastModified")
      )

instance Hashable ProvisionedConcurrencyConfigListItem

instance NFData ProvisionedConcurrencyConfigListItem
