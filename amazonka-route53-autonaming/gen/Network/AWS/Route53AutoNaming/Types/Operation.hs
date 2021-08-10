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
-- Module      : Network.AWS.Route53AutoNaming.Types.Operation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.Operation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.OperationStatus
import Network.AWS.Route53AutoNaming.Types.OperationTargetType
import Network.AWS.Route53AutoNaming.Types.OperationType

-- | A complex type that contains information about a specified operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The status of the operation. Values include the following:
    --
    -- -   __SUBMITTED__: This is the initial state immediately after you
    --     submit a request.
    --
    -- -   __PENDING__: AWS Cloud Map is performing the operation.
    --
    -- -   __SUCCESS__: The operation succeeded.
    --
    -- -   __FAIL__: The operation failed. For the failure reason, see
    --     @ErrorMessage@.
    status :: Prelude.Maybe OperationStatus,
    -- | The date and time that the request was submitted, in Unix date\/time
    -- format and Coordinated Universal Time (UTC). The value of @CreateDate@
    -- is accurate to milliseconds. For example, the value @1516925490.087@
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    createDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the operation that you want to get information about.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the target entity that is associated with the operation:
    --
    -- -   __NAMESPACE__: The namespace ID is returned in the @ResourceId@
    --     property.
    --
    -- -   __SERVICE__: The service ID is returned in the @ResourceId@
    --     property.
    --
    -- -   __INSTANCE__: The instance ID is returned in the @ResourceId@
    --     property.
    targets :: Prelude.Maybe (Prelude.HashMap OperationTargetType Prelude.Text),
    -- | If the value of @Status@ is @FAIL@, the reason that the operation
    -- failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation that is associated with the specified ID.
    type' :: Prelude.Maybe OperationType,
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
    -- | The date and time that the value of @Status@ changed to the current
    -- value, in Unix date\/time format and Coordinated Universal Time (UTC).
    -- The value of @UpdateDate@ is accurate to milliseconds. For example, the
    -- value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087
    -- AM.
    updateDate :: Prelude.Maybe Core.POSIX
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
-- 'status', 'operation_status' - The status of the operation. Values include the following:
--
-- -   __SUBMITTED__: This is the initial state immediately after you
--     submit a request.
--
-- -   __PENDING__: AWS Cloud Map is performing the operation.
--
-- -   __SUCCESS__: The operation succeeded.
--
-- -   __FAIL__: The operation failed. For the failure reason, see
--     @ErrorMessage@.
--
-- 'createDate', 'operation_createDate' - The date and time that the request was submitted, in Unix date\/time
-- format and Coordinated Universal Time (UTC). The value of @CreateDate@
-- is accurate to milliseconds. For example, the value @1516925490.087@
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'id', 'operation_id' - The ID of the operation that you want to get information about.
--
-- 'targets', 'operation_targets' - The name of the target entity that is associated with the operation:
--
-- -   __NAMESPACE__: The namespace ID is returned in the @ResourceId@
--     property.
--
-- -   __SERVICE__: The service ID is returned in the @ResourceId@
--     property.
--
-- -   __INSTANCE__: The instance ID is returned in the @ResourceId@
--     property.
--
-- 'errorMessage', 'operation_errorMessage' - If the value of @Status@ is @FAIL@, the reason that the operation
-- failed.
--
-- 'type'', 'operation_type' - The name of the operation that is associated with the specified ID.
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
-- 'updateDate', 'operation_updateDate' - The date and time that the value of @Status@ changed to the current
-- value, in Unix date\/time format and Coordinated Universal Time (UTC).
-- The value of @UpdateDate@ is accurate to milliseconds. For example, the
-- value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087
-- AM.
newOperation ::
  Operation
newOperation =
  Operation'
    { status = Prelude.Nothing,
      createDate = Prelude.Nothing,
      id = Prelude.Nothing,
      targets = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      type' = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      updateDate = Prelude.Nothing
    }

-- | The status of the operation. Values include the following:
--
-- -   __SUBMITTED__: This is the initial state immediately after you
--     submit a request.
--
-- -   __PENDING__: AWS Cloud Map is performing the operation.
--
-- -   __SUCCESS__: The operation succeeded.
--
-- -   __FAIL__: The operation failed. For the failure reason, see
--     @ErrorMessage@.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | The date and time that the request was submitted, in Unix date\/time
-- format and Coordinated Universal Time (UTC). The value of @CreateDate@
-- is accurate to milliseconds. For example, the value @1516925490.087@
-- represents Friday, January 26, 2018 12:11:30.087 AM.
operation_createDate :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_createDate = Lens.lens (\Operation' {createDate} -> createDate) (\s@Operation' {} a -> s {createDate = a} :: Operation) Prelude.. Lens.mapping Core._Time

-- | The ID of the operation that you want to get information about.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The name of the target entity that is associated with the operation:
--
-- -   __NAMESPACE__: The namespace ID is returned in the @ResourceId@
--     property.
--
-- -   __SERVICE__: The service ID is returned in the @ResourceId@
--     property.
--
-- -   __INSTANCE__: The instance ID is returned in the @ResourceId@
--     property.
operation_targets :: Lens.Lens' Operation (Prelude.Maybe (Prelude.HashMap OperationTargetType Prelude.Text))
operation_targets = Lens.lens (\Operation' {targets} -> targets) (\s@Operation' {} a -> s {targets = a} :: Operation) Prelude.. Lens.mapping Lens._Coerce

-- | If the value of @Status@ is @FAIL@, the reason that the operation
-- failed.
operation_errorMessage :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorMessage = Lens.lens (\Operation' {errorMessage} -> errorMessage) (\s@Operation' {} a -> s {errorMessage = a} :: Operation)

-- | The name of the operation that is associated with the specified ID.
operation_type :: Lens.Lens' Operation (Prelude.Maybe OperationType)
operation_type = Lens.lens (\Operation' {type'} -> type') (\s@Operation' {} a -> s {type' = a} :: Operation)

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

-- | The date and time that the value of @Status@ changed to the current
-- value, in Unix date\/time format and Coordinated Universal Time (UTC).
-- The value of @UpdateDate@ is accurate to milliseconds. For example, the
-- value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087
-- AM.
operation_updateDate :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_updateDate = Lens.lens (\Operation' {updateDate} -> updateDate) (\s@Operation' {} a -> s {updateDate = a} :: Operation) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Operation where
  parseJSON =
    Core.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "UpdateDate")
      )

instance Prelude.Hashable Operation

instance Prelude.NFData Operation
