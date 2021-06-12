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
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
import qualified Network.AWS.Lens as Lens

-- | Describes the error for a Reserved Instance whose queued purchase could
-- not be deleted.
--
-- /See:/ 'newDeleteQueuedReservedInstancesError' smart constructor.
data DeleteQueuedReservedInstancesError = DeleteQueuedReservedInstancesError'
  { -- | The error message.
    message :: Core.Maybe Core.Text,
    -- | The error code.
    code :: Core.Maybe DeleteQueuedReservedInstancesErrorCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteQueuedReservedInstancesError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'deleteQueuedReservedInstancesError_message' - The error message.
--
-- 'code', 'deleteQueuedReservedInstancesError_code' - The error code.
newDeleteQueuedReservedInstancesError ::
  DeleteQueuedReservedInstancesError
newDeleteQueuedReservedInstancesError =
  DeleteQueuedReservedInstancesError'
    { message =
        Core.Nothing,
      code = Core.Nothing
    }

-- | The error message.
deleteQueuedReservedInstancesError_message :: Lens.Lens' DeleteQueuedReservedInstancesError (Core.Maybe Core.Text)
deleteQueuedReservedInstancesError_message = Lens.lens (\DeleteQueuedReservedInstancesError' {message} -> message) (\s@DeleteQueuedReservedInstancesError' {} a -> s {message = a} :: DeleteQueuedReservedInstancesError)

-- | The error code.
deleteQueuedReservedInstancesError_code :: Lens.Lens' DeleteQueuedReservedInstancesError (Core.Maybe DeleteQueuedReservedInstancesErrorCode)
deleteQueuedReservedInstancesError_code = Lens.lens (\DeleteQueuedReservedInstancesError' {code} -> code) (\s@DeleteQueuedReservedInstancesError' {} a -> s {code = a} :: DeleteQueuedReservedInstancesError)

instance
  Core.FromXML
    DeleteQueuedReservedInstancesError
  where
  parseXML x =
    DeleteQueuedReservedInstancesError'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance
  Core.Hashable
    DeleteQueuedReservedInstancesError

instance
  Core.NFData
    DeleteQueuedReservedInstancesError
