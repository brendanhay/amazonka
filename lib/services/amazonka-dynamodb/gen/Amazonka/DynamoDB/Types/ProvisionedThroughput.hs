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
-- Module      : Amazonka.DynamoDB.Types.ProvisionedThroughput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ProvisionedThroughput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the @UpdateTable@ operation.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'newProvisionedThroughput' smart constructor.
data ProvisionedThroughput = ProvisionedThroughput'
  { -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- If read\/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
    readCapacityUnits :: Prelude.Natural,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- If read\/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
    writeCapacityUnits :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedThroughput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'provisionedThroughput_readCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If read\/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- 'writeCapacityUnits', 'provisionedThroughput_writeCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If read\/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
newProvisionedThroughput ::
  -- | 'readCapacityUnits'
  Prelude.Natural ->
  -- | 'writeCapacityUnits'
  Prelude.Natural ->
  ProvisionedThroughput
newProvisionedThroughput
  pReadCapacityUnits_
  pWriteCapacityUnits_ =
    ProvisionedThroughput'
      { readCapacityUnits =
          pReadCapacityUnits_,
        writeCapacityUnits = pWriteCapacityUnits_
      }

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If read\/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
provisionedThroughput_readCapacityUnits :: Lens.Lens' ProvisionedThroughput Prelude.Natural
provisionedThroughput_readCapacityUnits = Lens.lens (\ProvisionedThroughput' {readCapacityUnits} -> readCapacityUnits) (\s@ProvisionedThroughput' {} a -> s {readCapacityUnits = a} :: ProvisionedThroughput)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If read\/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
provisionedThroughput_writeCapacityUnits :: Lens.Lens' ProvisionedThroughput Prelude.Natural
provisionedThroughput_writeCapacityUnits = Lens.lens (\ProvisionedThroughput' {writeCapacityUnits} -> writeCapacityUnits) (\s@ProvisionedThroughput' {} a -> s {writeCapacityUnits = a} :: ProvisionedThroughput)

instance Core.FromJSON ProvisionedThroughput where
  parseJSON =
    Core.withObject
      "ProvisionedThroughput"
      ( \x ->
          ProvisionedThroughput'
            Prelude.<$> (x Core..: "ReadCapacityUnits")
            Prelude.<*> (x Core..: "WriteCapacityUnits")
      )

instance Prelude.Hashable ProvisionedThroughput where
  hashWithSalt _salt ProvisionedThroughput' {..} =
    _salt `Prelude.hashWithSalt` readCapacityUnits
      `Prelude.hashWithSalt` writeCapacityUnits

instance Prelude.NFData ProvisionedThroughput where
  rnf ProvisionedThroughput' {..} =
    Prelude.rnf readCapacityUnits
      `Prelude.seq` Prelude.rnf writeCapacityUnits

instance Core.ToJSON ProvisionedThroughput where
  toJSON ProvisionedThroughput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReadCapacityUnits" Core..= readCapacityUnits),
            Prelude.Just
              ("WriteCapacityUnits" Core..= writeCapacityUnits)
          ]
      )
