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
-- Module      : Amazonka.KeySpaces.Types.PointInTimeRecovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.PointInTimeRecovery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.PointInTimeRecoveryStatus
import qualified Amazonka.Prelude as Prelude

-- | Point-in-time recovery (PITR) helps protect your Amazon Keyspaces tables
-- from accidental write or delete operations by providing you continuous
-- backups of your table data.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/PointInTimeRecovery.html Point-in-time recovery>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- /See:/ 'newPointInTimeRecovery' smart constructor.
data PointInTimeRecovery = PointInTimeRecovery'
  { -- | The options are:
    --
    -- • @ENABLED@
    --
    -- • @DISABLED@
    status :: PointInTimeRecoveryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PointInTimeRecovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'pointInTimeRecovery_status' - The options are:
--
-- • @ENABLED@
--
-- • @DISABLED@
newPointInTimeRecovery ::
  -- | 'status'
  PointInTimeRecoveryStatus ->
  PointInTimeRecovery
newPointInTimeRecovery pStatus_ =
  PointInTimeRecovery' {status = pStatus_}

-- | The options are:
--
-- • @ENABLED@
--
-- • @DISABLED@
pointInTimeRecovery_status :: Lens.Lens' PointInTimeRecovery PointInTimeRecoveryStatus
pointInTimeRecovery_status = Lens.lens (\PointInTimeRecovery' {status} -> status) (\s@PointInTimeRecovery' {} a -> s {status = a} :: PointInTimeRecovery)

instance Prelude.Hashable PointInTimeRecovery where
  hashWithSalt _salt PointInTimeRecovery' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData PointInTimeRecovery where
  rnf PointInTimeRecovery' {..} = Prelude.rnf status

instance Data.ToJSON PointInTimeRecovery where
  toJSON PointInTimeRecovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )
