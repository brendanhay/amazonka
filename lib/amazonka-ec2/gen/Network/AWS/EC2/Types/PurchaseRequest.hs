{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PurchaseRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PurchaseRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a request to purchase Scheduled Instances.
--
--
--
-- /See:/ 'purchaseRequest' smart constructor.
data PurchaseRequest = PurchaseRequest'
  { _prInstanceCount :: !Int,
    _prPurchaseToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PurchaseRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prInstanceCount' - The number of instances.
--
-- * 'prPurchaseToken' - The purchase token.
purchaseRequest ::
  -- | 'prInstanceCount'
  Int ->
  -- | 'prPurchaseToken'
  Text ->
  PurchaseRequest
purchaseRequest pInstanceCount_ pPurchaseToken_ =
  PurchaseRequest'
    { _prInstanceCount = pInstanceCount_,
      _prPurchaseToken = pPurchaseToken_
    }

-- | The number of instances.
prInstanceCount :: Lens' PurchaseRequest Int
prInstanceCount = lens _prInstanceCount (\s a -> s {_prInstanceCount = a})

-- | The purchase token.
prPurchaseToken :: Lens' PurchaseRequest Text
prPurchaseToken = lens _prPurchaseToken (\s a -> s {_prPurchaseToken = a})

instance Hashable PurchaseRequest

instance NFData PurchaseRequest

instance ToQuery PurchaseRequest where
  toQuery PurchaseRequest' {..} =
    mconcat
      [ "InstanceCount" =: _prInstanceCount,
        "PurchaseToken" =: _prPurchaseToken
      ]
