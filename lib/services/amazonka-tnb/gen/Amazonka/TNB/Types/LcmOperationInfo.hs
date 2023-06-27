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
-- Module      : Amazonka.TNB.Types.LcmOperationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.LcmOperationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle management operation details on the network instance.
--
-- Lifecycle management operations are deploy, update, or delete
-- operations.
--
-- /See:/ 'newLcmOperationInfo' smart constructor.
data LcmOperationInfo = LcmOperationInfo'
  { -- | The identifier of the network operation.
    nsLcmOpOccId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LcmOperationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsLcmOpOccId', 'lcmOperationInfo_nsLcmOpOccId' - The identifier of the network operation.
newLcmOperationInfo ::
  -- | 'nsLcmOpOccId'
  Prelude.Text ->
  LcmOperationInfo
newLcmOperationInfo pNsLcmOpOccId_ =
  LcmOperationInfo' {nsLcmOpOccId = pNsLcmOpOccId_}

-- | The identifier of the network operation.
lcmOperationInfo_nsLcmOpOccId :: Lens.Lens' LcmOperationInfo Prelude.Text
lcmOperationInfo_nsLcmOpOccId = Lens.lens (\LcmOperationInfo' {nsLcmOpOccId} -> nsLcmOpOccId) (\s@LcmOperationInfo' {} a -> s {nsLcmOpOccId = a} :: LcmOperationInfo)

instance Data.FromJSON LcmOperationInfo where
  parseJSON =
    Data.withObject
      "LcmOperationInfo"
      ( \x ->
          LcmOperationInfo'
            Prelude.<$> (x Data..: "nsLcmOpOccId")
      )

instance Prelude.Hashable LcmOperationInfo where
  hashWithSalt _salt LcmOperationInfo' {..} =
    _salt `Prelude.hashWithSalt` nsLcmOpOccId

instance Prelude.NFData LcmOperationInfo where
  rnf LcmOperationInfo' {..} = Prelude.rnf nsLcmOpOccId
