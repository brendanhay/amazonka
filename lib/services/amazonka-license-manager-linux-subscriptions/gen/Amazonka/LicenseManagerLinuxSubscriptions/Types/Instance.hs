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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerLinuxSubscriptions.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details discovered information about a running instance using Linux
-- subscriptions.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The account ID which owns the instance.
    accountID :: Prelude.Maybe Prelude.Text,
    -- | The AMI ID used to launch the instance.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The instance ID of the resource.
    instanceID :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the resource.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The time in which the last discovery updated the instance details.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The product code for the instance. For more information, see
    -- <https://docs.aws.amazon.com/license-manager/latest/userguide/linux-subscriptions-usage-operation.html Usage operation values>
    -- in the /License Manager User Guide/ .
    productCode :: Prelude.Maybe [Prelude.Text],
    -- | The Region the instance is running in.
    region :: Prelude.Maybe Prelude.Text,
    -- | The status of the instance.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the subscription being used by the instance.
    subscriptionName :: Prelude.Maybe Prelude.Text,
    -- | The usage operation of the instance. For more information, see For more
    -- information, see
    -- <https://docs.aws.amazon.com/license-manager/latest/userguide/linux-subscriptions-usage-operation.html Usage operation values>
    -- in the /License Manager User Guide/.
    usageOperation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountID', 'instance_accountID' - The account ID which owns the instance.
--
-- 'amiId', 'instance_amiId' - The AMI ID used to launch the instance.
--
-- 'instanceID', 'instance_instanceID' - The instance ID of the resource.
--
-- 'instanceType', 'instance_instanceType' - The instance type of the resource.
--
-- 'lastUpdatedTime', 'instance_lastUpdatedTime' - The time in which the last discovery updated the instance details.
--
-- 'productCode', 'instance_productCode' - The product code for the instance. For more information, see
-- <https://docs.aws.amazon.com/license-manager/latest/userguide/linux-subscriptions-usage-operation.html Usage operation values>
-- in the /License Manager User Guide/ .
--
-- 'region', 'instance_region' - The Region the instance is running in.
--
-- 'status', 'instance_status' - The status of the instance.
--
-- 'subscriptionName', 'instance_subscriptionName' - The name of the subscription being used by the instance.
--
-- 'usageOperation', 'instance_usageOperation' - The usage operation of the instance. For more information, see For more
-- information, see
-- <https://docs.aws.amazon.com/license-manager/latest/userguide/linux-subscriptions-usage-operation.html Usage operation values>
-- in the /License Manager User Guide/.
newInstance ::
  Instance
newInstance =
  Instance'
    { accountID = Prelude.Nothing,
      amiId = Prelude.Nothing,
      instanceID = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      productCode = Prelude.Nothing,
      region = Prelude.Nothing,
      status = Prelude.Nothing,
      subscriptionName = Prelude.Nothing,
      usageOperation = Prelude.Nothing
    }

-- | The account ID which owns the instance.
instance_accountID :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_accountID = Lens.lens (\Instance' {accountID} -> accountID) (\s@Instance' {} a -> s {accountID = a} :: Instance)

-- | The AMI ID used to launch the instance.
instance_amiId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_amiId = Lens.lens (\Instance' {amiId} -> amiId) (\s@Instance' {} a -> s {amiId = a} :: Instance)

-- | The instance ID of the resource.
instance_instanceID :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceID = Lens.lens (\Instance' {instanceID} -> instanceID) (\s@Instance' {} a -> s {instanceID = a} :: Instance)

-- | The instance type of the resource.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The time in which the last discovery updated the instance details.
instance_lastUpdatedTime :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_lastUpdatedTime = Lens.lens (\Instance' {lastUpdatedTime} -> lastUpdatedTime) (\s@Instance' {} a -> s {lastUpdatedTime = a} :: Instance)

-- | The product code for the instance. For more information, see
-- <https://docs.aws.amazon.com/license-manager/latest/userguide/linux-subscriptions-usage-operation.html Usage operation values>
-- in the /License Manager User Guide/ .
instance_productCode :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_productCode = Lens.lens (\Instance' {productCode} -> productCode) (\s@Instance' {} a -> s {productCode = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The Region the instance is running in.
instance_region :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_region = Lens.lens (\Instance' {region} -> region) (\s@Instance' {} a -> s {region = a} :: Instance)

-- | The status of the instance.
instance_status :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

-- | The name of the subscription being used by the instance.
instance_subscriptionName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_subscriptionName = Lens.lens (\Instance' {subscriptionName} -> subscriptionName) (\s@Instance' {} a -> s {subscriptionName = a} :: Instance)

-- | The usage operation of the instance. For more information, see For more
-- information, see
-- <https://docs.aws.amazon.com/license-manager/latest/userguide/linux-subscriptions-usage-operation.html Usage operation values>
-- in the /License Manager User Guide/.
instance_usageOperation :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_usageOperation = Lens.lens (\Instance' {usageOperation} -> usageOperation) (\s@Instance' {} a -> s {usageOperation = a} :: Instance)

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "AccountID")
            Prelude.<*> (x Data..:? "AmiId")
            Prelude.<*> (x Data..:? "InstanceID")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "ProductCode" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubscriptionName")
            Prelude.<*> (x Data..:? "UsageOperation")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` accountID
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` instanceID
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` productCode
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subscriptionName
      `Prelude.hashWithSalt` usageOperation

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf accountID `Prelude.seq`
      Prelude.rnf amiId `Prelude.seq`
        Prelude.rnf instanceID `Prelude.seq`
          Prelude.rnf instanceType `Prelude.seq`
            Prelude.rnf lastUpdatedTime `Prelude.seq`
              Prelude.rnf productCode `Prelude.seq`
                Prelude.rnf region `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf subscriptionName `Prelude.seq`
                      Prelude.rnf usageOperation
