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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.AccessorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.AccessorStatus
import Amazonka.ManagedBlockChain.Types.AccessorType
import qualified Amazonka.Prelude as Prelude

-- | A summary of accessor properties.
--
-- /See:/ 'newAccessorSummary' smart constructor.
data AccessorSummary = AccessorSummary'
  { -- | The Amazon Resource Name (ARN) of the accessor. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time of the accessor.
    creationDate :: Prelude.Maybe Data.ISO8601,
    -- | The unique identifier of the accessor.
    id :: Prelude.Maybe Prelude.Text,
    -- | The current status of the accessor.
    status :: Prelude.Maybe AccessorStatus,
    -- | The type of the accessor.
    --
    -- Currently accessor type is restricted to @BILLING_TOKEN@.
    type' :: Prelude.Maybe AccessorType
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
-- 'arn', 'accessorSummary_arn' - The Amazon Resource Name (ARN) of the accessor. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'accessorSummary_creationDate' - The creation date and time of the accessor.
--
-- 'id', 'accessorSummary_id' - The unique identifier of the accessor.
--
-- 'status', 'accessorSummary_status' - The current status of the accessor.
--
-- 'type'', 'accessorSummary_type' - The type of the accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
newAccessorSummary ::
  AccessorSummary
newAccessorSummary =
  AccessorSummary'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the accessor. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
accessorSummary_arn :: Lens.Lens' AccessorSummary (Prelude.Maybe Prelude.Text)
accessorSummary_arn = Lens.lens (\AccessorSummary' {arn} -> arn) (\s@AccessorSummary' {} a -> s {arn = a} :: AccessorSummary)

-- | The creation date and time of the accessor.
accessorSummary_creationDate :: Lens.Lens' AccessorSummary (Prelude.Maybe Prelude.UTCTime)
accessorSummary_creationDate = Lens.lens (\AccessorSummary' {creationDate} -> creationDate) (\s@AccessorSummary' {} a -> s {creationDate = a} :: AccessorSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the accessor.
accessorSummary_id :: Lens.Lens' AccessorSummary (Prelude.Maybe Prelude.Text)
accessorSummary_id = Lens.lens (\AccessorSummary' {id} -> id) (\s@AccessorSummary' {} a -> s {id = a} :: AccessorSummary)

-- | The current status of the accessor.
accessorSummary_status :: Lens.Lens' AccessorSummary (Prelude.Maybe AccessorStatus)
accessorSummary_status = Lens.lens (\AccessorSummary' {status} -> status) (\s@AccessorSummary' {} a -> s {status = a} :: AccessorSummary)

-- | The type of the accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
accessorSummary_type :: Lens.Lens' AccessorSummary (Prelude.Maybe AccessorType)
accessorSummary_type = Lens.lens (\AccessorSummary' {type'} -> type') (\s@AccessorSummary' {} a -> s {type' = a} :: AccessorSummary)

instance Data.FromJSON AccessorSummary where
  parseJSON =
    Data.withObject
      "AccessorSummary"
      ( \x ->
          AccessorSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AccessorSummary where
  hashWithSalt _salt AccessorSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AccessorSummary where
  rnf AccessorSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
