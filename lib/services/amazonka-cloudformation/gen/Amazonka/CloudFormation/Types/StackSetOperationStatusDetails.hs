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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationStatusDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationStatusDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the StackSet operation.
--
-- /See:/ 'newStackSetOperationStatusDetails' smart constructor.
data StackSetOperationStatusDetails = StackSetOperationStatusDetails'
  { -- | The number of stack instances for which the StackSet operation failed.
    failedStackInstancesCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetOperationStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedStackInstancesCount', 'stackSetOperationStatusDetails_failedStackInstancesCount' - The number of stack instances for which the StackSet operation failed.
newStackSetOperationStatusDetails ::
  StackSetOperationStatusDetails
newStackSetOperationStatusDetails =
  StackSetOperationStatusDetails'
    { failedStackInstancesCount =
        Prelude.Nothing
    }

-- | The number of stack instances for which the StackSet operation failed.
stackSetOperationStatusDetails_failedStackInstancesCount :: Lens.Lens' StackSetOperationStatusDetails (Prelude.Maybe Prelude.Natural)
stackSetOperationStatusDetails_failedStackInstancesCount = Lens.lens (\StackSetOperationStatusDetails' {failedStackInstancesCount} -> failedStackInstancesCount) (\s@StackSetOperationStatusDetails' {} a -> s {failedStackInstancesCount = a} :: StackSetOperationStatusDetails)

instance Data.FromXML StackSetOperationStatusDetails where
  parseXML x =
    StackSetOperationStatusDetails'
      Prelude.<$> (x Data..@? "FailedStackInstancesCount")

instance
  Prelude.Hashable
    StackSetOperationStatusDetails
  where
  hashWithSalt
    _salt
    StackSetOperationStatusDetails' {..} =
      _salt
        `Prelude.hashWithSalt` failedStackInstancesCount

instance
  Prelude.NFData
    StackSetOperationStatusDetails
  where
  rnf StackSetOperationStatusDetails' {..} =
    Prelude.rnf failedStackInstancesCount
