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
-- Module      : Amazonka.TNB.Types.ListSolNetworkOperationsInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolNetworkOperationsInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.LcmOperationType
import Amazonka.TNB.Types.ListSolNetworkOperationsMetadata
import Amazonka.TNB.Types.NsLcmOperationState
import Amazonka.TNB.Types.ProblemDetails

-- | Information parameters for a network operation.
--
-- /See:/ 'newListSolNetworkOperationsInfo' smart constructor.
data ListSolNetworkOperationsInfo = ListSolNetworkOperationsInfo'
  { -- | Error related to this specific network operation.
    error :: Prelude.Maybe ProblemDetails,
    -- | Metadata related to this network operation.
    metadata :: Prelude.Maybe ListSolNetworkOperationsMetadata,
    -- | Network operation ARN.
    arn :: Prelude.Text,
    -- | ID of this network operation.
    id :: Prelude.Text,
    -- | Type of lifecycle management network operation.
    lcmOperationType :: LcmOperationType,
    -- | ID of the network instance related to this operation.
    nsInstanceId :: Prelude.Text,
    -- | The state of the network operation.
    operationState :: NsLcmOperationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkOperationsInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'listSolNetworkOperationsInfo_error' - Error related to this specific network operation.
--
-- 'metadata', 'listSolNetworkOperationsInfo_metadata' - Metadata related to this network operation.
--
-- 'arn', 'listSolNetworkOperationsInfo_arn' - Network operation ARN.
--
-- 'id', 'listSolNetworkOperationsInfo_id' - ID of this network operation.
--
-- 'lcmOperationType', 'listSolNetworkOperationsInfo_lcmOperationType' - Type of lifecycle management network operation.
--
-- 'nsInstanceId', 'listSolNetworkOperationsInfo_nsInstanceId' - ID of the network instance related to this operation.
--
-- 'operationState', 'listSolNetworkOperationsInfo_operationState' - The state of the network operation.
newListSolNetworkOperationsInfo ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lcmOperationType'
  LcmOperationType ->
  -- | 'nsInstanceId'
  Prelude.Text ->
  -- | 'operationState'
  NsLcmOperationState ->
  ListSolNetworkOperationsInfo
newListSolNetworkOperationsInfo
  pArn_
  pId_
  pLcmOperationType_
  pNsInstanceId_
  pOperationState_ =
    ListSolNetworkOperationsInfo'
      { error =
          Prelude.Nothing,
        metadata = Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        lcmOperationType = pLcmOperationType_,
        nsInstanceId = pNsInstanceId_,
        operationState = pOperationState_
      }

-- | Error related to this specific network operation.
listSolNetworkOperationsInfo_error :: Lens.Lens' ListSolNetworkOperationsInfo (Prelude.Maybe ProblemDetails)
listSolNetworkOperationsInfo_error = Lens.lens (\ListSolNetworkOperationsInfo' {error} -> error) (\s@ListSolNetworkOperationsInfo' {} a -> s {error = a} :: ListSolNetworkOperationsInfo)

-- | Metadata related to this network operation.
listSolNetworkOperationsInfo_metadata :: Lens.Lens' ListSolNetworkOperationsInfo (Prelude.Maybe ListSolNetworkOperationsMetadata)
listSolNetworkOperationsInfo_metadata = Lens.lens (\ListSolNetworkOperationsInfo' {metadata} -> metadata) (\s@ListSolNetworkOperationsInfo' {} a -> s {metadata = a} :: ListSolNetworkOperationsInfo)

-- | Network operation ARN.
listSolNetworkOperationsInfo_arn :: Lens.Lens' ListSolNetworkOperationsInfo Prelude.Text
listSolNetworkOperationsInfo_arn = Lens.lens (\ListSolNetworkOperationsInfo' {arn} -> arn) (\s@ListSolNetworkOperationsInfo' {} a -> s {arn = a} :: ListSolNetworkOperationsInfo)

-- | ID of this network operation.
listSolNetworkOperationsInfo_id :: Lens.Lens' ListSolNetworkOperationsInfo Prelude.Text
listSolNetworkOperationsInfo_id = Lens.lens (\ListSolNetworkOperationsInfo' {id} -> id) (\s@ListSolNetworkOperationsInfo' {} a -> s {id = a} :: ListSolNetworkOperationsInfo)

-- | Type of lifecycle management network operation.
listSolNetworkOperationsInfo_lcmOperationType :: Lens.Lens' ListSolNetworkOperationsInfo LcmOperationType
listSolNetworkOperationsInfo_lcmOperationType = Lens.lens (\ListSolNetworkOperationsInfo' {lcmOperationType} -> lcmOperationType) (\s@ListSolNetworkOperationsInfo' {} a -> s {lcmOperationType = a} :: ListSolNetworkOperationsInfo)

-- | ID of the network instance related to this operation.
listSolNetworkOperationsInfo_nsInstanceId :: Lens.Lens' ListSolNetworkOperationsInfo Prelude.Text
listSolNetworkOperationsInfo_nsInstanceId = Lens.lens (\ListSolNetworkOperationsInfo' {nsInstanceId} -> nsInstanceId) (\s@ListSolNetworkOperationsInfo' {} a -> s {nsInstanceId = a} :: ListSolNetworkOperationsInfo)

-- | The state of the network operation.
listSolNetworkOperationsInfo_operationState :: Lens.Lens' ListSolNetworkOperationsInfo NsLcmOperationState
listSolNetworkOperationsInfo_operationState = Lens.lens (\ListSolNetworkOperationsInfo' {operationState} -> operationState) (\s@ListSolNetworkOperationsInfo' {} a -> s {operationState = a} :: ListSolNetworkOperationsInfo)

instance Data.FromJSON ListSolNetworkOperationsInfo where
  parseJSON =
    Data.withObject
      "ListSolNetworkOperationsInfo"
      ( \x ->
          ListSolNetworkOperationsInfo'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "metadata")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "lcmOperationType")
            Prelude.<*> (x Data..: "nsInstanceId")
            Prelude.<*> (x Data..: "operationState")
      )

instance
  Prelude.Hashable
    ListSolNetworkOperationsInfo
  where
  hashWithSalt _salt ListSolNetworkOperationsInfo' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lcmOperationType
      `Prelude.hashWithSalt` nsInstanceId
      `Prelude.hashWithSalt` operationState

instance Prelude.NFData ListSolNetworkOperationsInfo where
  rnf ListSolNetworkOperationsInfo' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lcmOperationType
      `Prelude.seq` Prelude.rnf nsInstanceId
      `Prelude.seq` Prelude.rnf operationState
