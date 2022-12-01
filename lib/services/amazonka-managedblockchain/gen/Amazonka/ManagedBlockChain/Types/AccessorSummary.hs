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
-- Module      : Amazonka.ManagedBlockChain.Types.AccessorSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.AccessorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ManagedBlockChain.Types.AccessorStatus
import Amazonka.ManagedBlockChain.Types.AccessorType
import qualified Amazonka.Prelude as Prelude

-- | The token based access feature is in preview release for Ethereum on
-- Amazon Managed Blockchain and is subject to change. We recommend that
-- you use this feature only with test scenarios, and not in production
-- environments.
--
-- A summary of accessor properties.
--
-- /See:/ 'newAccessorSummary' smart constructor.
data AccessorSummary = AccessorSummary'
  { -- | The type of the accessor.
    --
    -- Currently accessor type is restricted to @BILLING_TOKEN@.
    type' :: Prelude.Maybe AccessorType,
    -- | The Amazon Resource Name (ARN) of the accessor. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time of the accessor.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The current status of the accessor.
    status :: Prelude.Maybe AccessorStatus,
    -- | The unique identifier of the accessor.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'accessorSummary_type' - The type of the accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
--
-- 'arn', 'accessorSummary_arn' - The Amazon Resource Name (ARN) of the accessor. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'accessorSummary_creationDate' - The creation date and time of the accessor.
--
-- 'status', 'accessorSummary_status' - The current status of the accessor.
--
-- 'id', 'accessorSummary_id' - The unique identifier of the accessor.
newAccessorSummary ::
  AccessorSummary
newAccessorSummary =
  AccessorSummary'
    { type' = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The type of the accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
accessorSummary_type :: Lens.Lens' AccessorSummary (Prelude.Maybe AccessorType)
accessorSummary_type = Lens.lens (\AccessorSummary' {type'} -> type') (\s@AccessorSummary' {} a -> s {type' = a} :: AccessorSummary)

-- | The Amazon Resource Name (ARN) of the accessor. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
accessorSummary_arn :: Lens.Lens' AccessorSummary (Prelude.Maybe Prelude.Text)
accessorSummary_arn = Lens.lens (\AccessorSummary' {arn} -> arn) (\s@AccessorSummary' {} a -> s {arn = a} :: AccessorSummary)

-- | The creation date and time of the accessor.
accessorSummary_creationDate :: Lens.Lens' AccessorSummary (Prelude.Maybe Prelude.UTCTime)
accessorSummary_creationDate = Lens.lens (\AccessorSummary' {creationDate} -> creationDate) (\s@AccessorSummary' {} a -> s {creationDate = a} :: AccessorSummary) Prelude.. Lens.mapping Core._Time

-- | The current status of the accessor.
accessorSummary_status :: Lens.Lens' AccessorSummary (Prelude.Maybe AccessorStatus)
accessorSummary_status = Lens.lens (\AccessorSummary' {status} -> status) (\s@AccessorSummary' {} a -> s {status = a} :: AccessorSummary)

-- | The unique identifier of the accessor.
accessorSummary_id :: Lens.Lens' AccessorSummary (Prelude.Maybe Prelude.Text)
accessorSummary_id = Lens.lens (\AccessorSummary' {id} -> id) (\s@AccessorSummary' {} a -> s {id = a} :: AccessorSummary)

instance Core.FromJSON AccessorSummary where
  parseJSON =
    Core.withObject
      "AccessorSummary"
      ( \x ->
          AccessorSummary'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable AccessorSummary where
  hashWithSalt _salt AccessorSummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id

instance Prelude.NFData AccessorSummary where
  rnf AccessorSummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
