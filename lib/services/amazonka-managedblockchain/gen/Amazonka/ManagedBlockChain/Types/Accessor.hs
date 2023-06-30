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
-- Module      : Amazonka.ManagedBlockChain.Types.Accessor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Accessor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.AccessorStatus
import Amazonka.ManagedBlockChain.Types.AccessorType
import qualified Amazonka.Prelude as Prelude

-- | The token based access feature is in preview release for Ethereum on
-- Amazon Managed Blockchain and is subject to change. We recommend that
-- you use this feature only with test scenarios, and not in production
-- environments.
--
-- The properties of the Accessor.
--
-- /See:/ 'newAccessor' smart constructor.
data Accessor = Accessor'
  { -- | The Amazon Resource Name (ARN) of the accessor. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The billing token is a property of the accessor. Use this token to make
    -- Ethereum API calls to your Ethereum node. The billing token is used to
    -- track your accessor object for billing Ethereum API requests made to
    -- your Ethereum nodes.
    billingToken :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'Accessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'accessor_arn' - The Amazon Resource Name (ARN) of the accessor. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'billingToken', 'accessor_billingToken' - The billing token is a property of the accessor. Use this token to make
-- Ethereum API calls to your Ethereum node. The billing token is used to
-- track your accessor object for billing Ethereum API requests made to
-- your Ethereum nodes.
--
-- 'creationDate', 'accessor_creationDate' - The creation date and time of the accessor.
--
-- 'id', 'accessor_id' - The unique identifier of the accessor.
--
-- 'status', 'accessor_status' - The current status of the accessor.
--
-- 'type'', 'accessor_type' - The type of the accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
newAccessor ::
  Accessor
newAccessor =
  Accessor'
    { arn = Prelude.Nothing,
      billingToken = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the accessor. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
accessor_arn :: Lens.Lens' Accessor (Prelude.Maybe Prelude.Text)
accessor_arn = Lens.lens (\Accessor' {arn} -> arn) (\s@Accessor' {} a -> s {arn = a} :: Accessor)

-- | The billing token is a property of the accessor. Use this token to make
-- Ethereum API calls to your Ethereum node. The billing token is used to
-- track your accessor object for billing Ethereum API requests made to
-- your Ethereum nodes.
accessor_billingToken :: Lens.Lens' Accessor (Prelude.Maybe Prelude.Text)
accessor_billingToken = Lens.lens (\Accessor' {billingToken} -> billingToken) (\s@Accessor' {} a -> s {billingToken = a} :: Accessor)

-- | The creation date and time of the accessor.
accessor_creationDate :: Lens.Lens' Accessor (Prelude.Maybe Prelude.UTCTime)
accessor_creationDate = Lens.lens (\Accessor' {creationDate} -> creationDate) (\s@Accessor' {} a -> s {creationDate = a} :: Accessor) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the accessor.
accessor_id :: Lens.Lens' Accessor (Prelude.Maybe Prelude.Text)
accessor_id = Lens.lens (\Accessor' {id} -> id) (\s@Accessor' {} a -> s {id = a} :: Accessor)

-- | The current status of the accessor.
accessor_status :: Lens.Lens' Accessor (Prelude.Maybe AccessorStatus)
accessor_status = Lens.lens (\Accessor' {status} -> status) (\s@Accessor' {} a -> s {status = a} :: Accessor)

-- | The type of the accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
accessor_type :: Lens.Lens' Accessor (Prelude.Maybe AccessorType)
accessor_type = Lens.lens (\Accessor' {type'} -> type') (\s@Accessor' {} a -> s {type' = a} :: Accessor)

instance Data.FromJSON Accessor where
  parseJSON =
    Data.withObject
      "Accessor"
      ( \x ->
          Accessor'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "BillingToken")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Accessor where
  hashWithSalt _salt Accessor' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` billingToken
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Accessor where
  rnf Accessor' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf billingToken
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
