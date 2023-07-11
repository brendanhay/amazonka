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
-- Module      : Amazonka.Route53AutoNaming.Types.Operation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.Operation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.OperationStatus
import Amazonka.Route53AutoNaming.Types.OperationTargetType
import Amazonka.Route53AutoNaming.Types.OperationType

-- | A complex type that contains information about a specified operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The date and time that the request was submitted, in Unix date\/time
    -- format and Coordinated Universal Time (UTC). The value of @CreateDate@
    -- is accurate to milliseconds. For example, the value @1516925490.087@
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The code associated with @ErrorMessage@. Values for @ErrorCode@ include
    -- the following:
    --
    -- -   @ACCESS_DENIED@
    --
    -- -   @CANNOT_CREATE_HOSTED_ZONE@
    --
    -- -   @EXPIRED_TOKEN@
    --
    -- -   @HOSTED_ZONE_NOT_FOUND@
    --
    -- -   @INTERNAL_FAILURE@
    --
    -- -   @INVALID_CHANGE_BATCH@
    --
    -- -   @THROTTLED_REQUEST@
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | If the value of @Status@ is @FAIL@, the reason that the operation
    -- failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the operation that you want to get information about.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of the operation. Values include the following:
    --
    -- [SUBMITTED]
    --     This is the initial state that occurs immediately after you submit a
    --     request.
    --
    -- [PENDING]
    --     Cloud Map is performing the operation.
    --
    -- [SUCCESS]
    --     The operation succeeded.
    --
    -- [FAIL]
    --     The operation failed. For the failure reason, see @ErrorMessage@.
    status :: Prelude.Maybe OperationStatus,
    -- | The name of the target entity that\'s associated with the operation:
    --
    -- [NAMESPACE]
    --     The namespace ID is returned in the @ResourceId@ property.
    --
    -- [SERVICE]
    --     The service ID is returned in the @ResourceId@ property.
    --
    -- [INSTANCE]
    --     The instance ID is returned in the @ResourceId@ property.
    targets :: Prelude.Maybe (Prelude.HashMap OperationTargetType Prelude.Text),
    -- | The name of the operation that\'s associated with the specified ID.
    type' :: Prelude.Maybe OperationType,
    -- | The date and time that the value of @Status@ changed to the current
    -- value, in Unix date\/time format and Coordinated Universal Time (UTC).
    -- The value of @UpdateDate@ is accurate to milliseconds. For example, the
    -- value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087
    -- AM.
    updateDate :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Operation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'operation_createDate' - The date and time that the request was submitted, in Unix date\/time
-- format and Coordinated Universal Time (UTC). The value of @CreateDate@
-- is accurate to milliseconds. For example, the value @1516925490.087@
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'errorCode', 'operation_errorCode' - The code associated with @ErrorMessage@. Values for @ErrorCode@ include
-- the following:
--
-- -   @ACCESS_DENIED@
--
-- -   @CANNOT_CREATE_HOSTED_ZONE@
--
-- -   @EXPIRED_TOKEN@
--
-- -   @HOSTED_ZONE_NOT_FOUND@
--
-- -   @INTERNAL_FAILURE@
--
-- -   @INVALID_CHANGE_BATCH@
--
-- -   @THROTTLED_REQUEST@
--
-- 'errorMessage', 'operation_errorMessage' - If the value of @Status@ is @FAIL@, the reason that the operation
-- failed.
--
-- 'id', 'operation_id' - The ID of the operation that you want to get information about.
--
-- 'status', 'operation_status' - The status of the operation. Values include the following:
--
-- [SUBMITTED]
--     This is the initial state that occurs immediately after you submit a
--     request.
--
-- [PENDING]
--     Cloud Map is performing the operation.
--
-- [SUCCESS]
--     The operation succeeded.
--
-- [FAIL]
--     The operation failed. For the failure reason, see @ErrorMessage@.
--
-- 'targets', 'operation_targets' - The name of the target entity that\'s associated with the operation:
--
-- [NAMESPACE]
--     The namespace ID is returned in the @ResourceId@ property.
--
-- [SERVICE]
--     The service ID is returned in the @ResourceId@ property.
--
-- [INSTANCE]
--     The instance ID is returned in the @ResourceId@ property.
--
-- 'type'', 'operation_type' - The name of the operation that\'s associated with the specified ID.
--
-- 'updateDate', 'operation_updateDate' - The date and time that the value of @Status@ changed to the current
-- value, in Unix date\/time format and Coordinated Universal Time (UTC).
-- The value of @UpdateDate@ is accurate to milliseconds. For example, the
-- value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087
-- AM.
newOperation ::
  Operation
newOperation =
  Operation'
    { createDate = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing,
      targets = Prelude.Nothing,
      type' = Prelude.Nothing,
      updateDate = Prelude.Nothing
    }

-- | The date and time that the request was submitted, in Unix date\/time
-- format and Coordinated Universal Time (UTC). The value of @CreateDate@
-- is accurate to milliseconds. For example, the value @1516925490.087@
-- represents Friday, January 26, 2018 12:11:30.087 AM.
operation_createDate :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_createDate = Lens.lens (\Operation' {createDate} -> createDate) (\s@Operation' {} a -> s {createDate = a} :: Operation) Prelude.. Lens.mapping Data._Time

-- | The code associated with @ErrorMessage@. Values for @ErrorCode@ include
-- the following:
--
-- -   @ACCESS_DENIED@
--
-- -   @CANNOT_CREATE_HOSTED_ZONE@
--
-- -   @EXPIRED_TOKEN@
--
-- -   @HOSTED_ZONE_NOT_FOUND@
--
-- -   @INTERNAL_FAILURE@
--
-- -   @INVALID_CHANGE_BATCH@
--
-- -   @THROTTLED_REQUEST@
operation_errorCode :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorCode = Lens.lens (\Operation' {errorCode} -> errorCode) (\s@Operation' {} a -> s {errorCode = a} :: Operation)

-- | If the value of @Status@ is @FAIL@, the reason that the operation
-- failed.
operation_errorMessage :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorMessage = Lens.lens (\Operation' {errorMessage} -> errorMessage) (\s@Operation' {} a -> s {errorMessage = a} :: Operation)

-- | The ID of the operation that you want to get information about.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The status of the operation. Values include the following:
--
-- [SUBMITTED]
--     This is the initial state that occurs immediately after you submit a
--     request.
--
-- [PENDING]
--     Cloud Map is performing the operation.
--
-- [SUCCESS]
--     The operation succeeded.
--
-- [FAIL]
--     The operation failed. For the failure reason, see @ErrorMessage@.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | The name of the target entity that\'s associated with the operation:
--
-- [NAMESPACE]
--     The namespace ID is returned in the @ResourceId@ property.
--
-- [SERVICE]
--     The service ID is returned in the @ResourceId@ property.
--
-- [INSTANCE]
--     The instance ID is returned in the @ResourceId@ property.
operation_targets :: Lens.Lens' Operation (Prelude.Maybe (Prelude.HashMap OperationTargetType Prelude.Text))
operation_targets = Lens.lens (\Operation' {targets} -> targets) (\s@Operation' {} a -> s {targets = a} :: Operation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the operation that\'s associated with the specified ID.
operation_type :: Lens.Lens' Operation (Prelude.Maybe OperationType)
operation_type = Lens.lens (\Operation' {type'} -> type') (\s@Operation' {} a -> s {type' = a} :: Operation)

-- | The date and time that the value of @Status@ changed to the current
-- value, in Unix date\/time format and Coordinated Universal Time (UTC).
-- The value of @UpdateDate@ is accurate to milliseconds. For example, the
-- value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087
-- AM.
operation_updateDate :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_updateDate = Lens.lens (\Operation' {updateDate} -> updateDate) (\s@Operation' {} a -> s {updateDate = a} :: Operation) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Operation where
  parseJSON =
    Data.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "UpdateDate")
      )

instance Prelude.Hashable Operation where
  hashWithSalt _salt Operation' {..} =
    _salt
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updateDate

instance Prelude.NFData Operation where
  rnf Operation' {..} =
    Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updateDate
