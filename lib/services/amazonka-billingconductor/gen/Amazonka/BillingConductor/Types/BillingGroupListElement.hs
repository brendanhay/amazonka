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
-- Module      : Amazonka.BillingConductor.Types.BillingGroupListElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.BillingGroupListElement where

import Amazonka.BillingConductor.Types.BillingGroupStatus
import Amazonka.BillingConductor.Types.ComputationPreference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of a billing group.
--
-- /See:/ 'newBillingGroupListElement' smart constructor.
data BillingGroupListElement = BillingGroupListElement'
  { -- | The Amazon Resource Number (ARN) that can be used to uniquely identify
    -- the billing group.
    arn :: Prelude.Maybe Prelude.Text,
    computationPreference :: Prelude.Maybe ComputationPreference,
    -- | The time when the billing group was created.
    creationTime :: Prelude.Maybe Prelude.Integer,
    -- | The description of the billing group.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time when the billing group was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of the billing group.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The account ID that serves as the main account in a billing group.
    primaryAccountId :: Prelude.Maybe Prelude.Text,
    -- | The number of accounts in the particular billing group.
    size :: Prelude.Maybe Prelude.Natural,
    -- | The billing group status. Only one of the valid values can be used.
    status :: Prelude.Maybe BillingGroupStatus,
    -- | The reason why the billing group is in its current status.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BillingGroupListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'billingGroupListElement_arn' - The Amazon Resource Number (ARN) that can be used to uniquely identify
-- the billing group.
--
-- 'computationPreference', 'billingGroupListElement_computationPreference' - Undocumented member.
--
-- 'creationTime', 'billingGroupListElement_creationTime' - The time when the billing group was created.
--
-- 'description', 'billingGroupListElement_description' - The description of the billing group.
--
-- 'lastModifiedTime', 'billingGroupListElement_lastModifiedTime' - The most recent time when the billing group was modified.
--
-- 'name', 'billingGroupListElement_name' - The name of the billing group.
--
-- 'primaryAccountId', 'billingGroupListElement_primaryAccountId' - The account ID that serves as the main account in a billing group.
--
-- 'size', 'billingGroupListElement_size' - The number of accounts in the particular billing group.
--
-- 'status', 'billingGroupListElement_status' - The billing group status. Only one of the valid values can be used.
--
-- 'statusReason', 'billingGroupListElement_statusReason' - The reason why the billing group is in its current status.
newBillingGroupListElement ::
  BillingGroupListElement
newBillingGroupListElement =
  BillingGroupListElement'
    { arn = Prelude.Nothing,
      computationPreference = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      primaryAccountId = Prelude.Nothing,
      size = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The Amazon Resource Number (ARN) that can be used to uniquely identify
-- the billing group.
billingGroupListElement_arn :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Text)
billingGroupListElement_arn = Lens.lens (\BillingGroupListElement' {arn} -> arn) (\s@BillingGroupListElement' {} a -> s {arn = a} :: BillingGroupListElement)

-- | Undocumented member.
billingGroupListElement_computationPreference :: Lens.Lens' BillingGroupListElement (Prelude.Maybe ComputationPreference)
billingGroupListElement_computationPreference = Lens.lens (\BillingGroupListElement' {computationPreference} -> computationPreference) (\s@BillingGroupListElement' {} a -> s {computationPreference = a} :: BillingGroupListElement)

-- | The time when the billing group was created.
billingGroupListElement_creationTime :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Integer)
billingGroupListElement_creationTime = Lens.lens (\BillingGroupListElement' {creationTime} -> creationTime) (\s@BillingGroupListElement' {} a -> s {creationTime = a} :: BillingGroupListElement)

-- | The description of the billing group.
billingGroupListElement_description :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Text)
billingGroupListElement_description = Lens.lens (\BillingGroupListElement' {description} -> description) (\s@BillingGroupListElement' {} a -> s {description = a} :: BillingGroupListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time when the billing group was modified.
billingGroupListElement_lastModifiedTime :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Integer)
billingGroupListElement_lastModifiedTime = Lens.lens (\BillingGroupListElement' {lastModifiedTime} -> lastModifiedTime) (\s@BillingGroupListElement' {} a -> s {lastModifiedTime = a} :: BillingGroupListElement)

-- | The name of the billing group.
billingGroupListElement_name :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Text)
billingGroupListElement_name = Lens.lens (\BillingGroupListElement' {name} -> name) (\s@BillingGroupListElement' {} a -> s {name = a} :: BillingGroupListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The account ID that serves as the main account in a billing group.
billingGroupListElement_primaryAccountId :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Text)
billingGroupListElement_primaryAccountId = Lens.lens (\BillingGroupListElement' {primaryAccountId} -> primaryAccountId) (\s@BillingGroupListElement' {} a -> s {primaryAccountId = a} :: BillingGroupListElement)

-- | The number of accounts in the particular billing group.
billingGroupListElement_size :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Natural)
billingGroupListElement_size = Lens.lens (\BillingGroupListElement' {size} -> size) (\s@BillingGroupListElement' {} a -> s {size = a} :: BillingGroupListElement)

-- | The billing group status. Only one of the valid values can be used.
billingGroupListElement_status :: Lens.Lens' BillingGroupListElement (Prelude.Maybe BillingGroupStatus)
billingGroupListElement_status = Lens.lens (\BillingGroupListElement' {status} -> status) (\s@BillingGroupListElement' {} a -> s {status = a} :: BillingGroupListElement)

-- | The reason why the billing group is in its current status.
billingGroupListElement_statusReason :: Lens.Lens' BillingGroupListElement (Prelude.Maybe Prelude.Text)
billingGroupListElement_statusReason = Lens.lens (\BillingGroupListElement' {statusReason} -> statusReason) (\s@BillingGroupListElement' {} a -> s {statusReason = a} :: BillingGroupListElement)

instance Data.FromJSON BillingGroupListElement where
  parseJSON =
    Data.withObject
      "BillingGroupListElement"
      ( \x ->
          BillingGroupListElement'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ComputationPreference")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PrimaryAccountId")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReason")
      )

instance Prelude.Hashable BillingGroupListElement where
  hashWithSalt _salt BillingGroupListElement' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` computationPreference
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` primaryAccountId
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData BillingGroupListElement where
  rnf BillingGroupListElement' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf computationPreference `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf primaryAccountId `Prelude.seq`
                  Prelude.rnf size `Prelude.seq`
                    Prelude.rnf status `Prelude.seq`
                      Prelude.rnf statusReason
