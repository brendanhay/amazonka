{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PurchaseRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PurchaseRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a request to purchase Scheduled Instances.
--
-- /See:/ 'newPurchaseRequest' smart constructor.
data PurchaseRequest = PurchaseRequest'
  { -- | The number of instances.
    instanceCount :: Prelude.Int,
    -- | The purchase token.
    purchaseToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PurchaseRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'purchaseRequest_instanceCount' - The number of instances.
--
-- 'purchaseToken', 'purchaseRequest_purchaseToken' - The purchase token.
newPurchaseRequest ::
  -- | 'instanceCount'
  Prelude.Int ->
  -- | 'purchaseToken'
  Prelude.Text ->
  PurchaseRequest
newPurchaseRequest pInstanceCount_ pPurchaseToken_ =
  PurchaseRequest'
    { instanceCount = pInstanceCount_,
      purchaseToken = pPurchaseToken_
    }

-- | The number of instances.
purchaseRequest_instanceCount :: Lens.Lens' PurchaseRequest Prelude.Int
purchaseRequest_instanceCount = Lens.lens (\PurchaseRequest' {instanceCount} -> instanceCount) (\s@PurchaseRequest' {} a -> s {instanceCount = a} :: PurchaseRequest)

-- | The purchase token.
purchaseRequest_purchaseToken :: Lens.Lens' PurchaseRequest Prelude.Text
purchaseRequest_purchaseToken = Lens.lens (\PurchaseRequest' {purchaseToken} -> purchaseToken) (\s@PurchaseRequest' {} a -> s {purchaseToken = a} :: PurchaseRequest)

instance Prelude.Hashable PurchaseRequest

instance Prelude.NFData PurchaseRequest

instance Prelude.ToQuery PurchaseRequest where
  toQuery PurchaseRequest' {..} =
    Prelude.mconcat
      [ "InstanceCount" Prelude.=: instanceCount,
        "PurchaseToken" Prelude.=: purchaseToken
      ]
