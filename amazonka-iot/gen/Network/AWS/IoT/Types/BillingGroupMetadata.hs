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
-- Module      : Network.AWS.IoT.Types.BillingGroupMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BillingGroupMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Additional information about the billing group.
--
-- /See:/ 'newBillingGroupMetadata' smart constructor.
data BillingGroupMetadata = BillingGroupMetadata'
  { -- | The date the billing group was created.
    creationDate :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BillingGroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'billingGroupMetadata_creationDate' - The date the billing group was created.
newBillingGroupMetadata ::
  BillingGroupMetadata
newBillingGroupMetadata =
  BillingGroupMetadata'
    { creationDate =
        Prelude.Nothing
    }

-- | The date the billing group was created.
billingGroupMetadata_creationDate :: Lens.Lens' BillingGroupMetadata (Prelude.Maybe Prelude.UTCTime)
billingGroupMetadata_creationDate = Lens.lens (\BillingGroupMetadata' {creationDate} -> creationDate) (\s@BillingGroupMetadata' {} a -> s {creationDate = a} :: BillingGroupMetadata) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON BillingGroupMetadata where
  parseJSON =
    Prelude.withObject
      "BillingGroupMetadata"
      ( \x ->
          BillingGroupMetadata'
            Prelude.<$> (x Prelude..:? "creationDate")
      )

instance Prelude.Hashable BillingGroupMetadata

instance Prelude.NFData BillingGroupMetadata
