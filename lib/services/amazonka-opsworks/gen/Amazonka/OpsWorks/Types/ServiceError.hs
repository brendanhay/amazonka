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
-- Module      : Amazonka.OpsWorks.Types.ServiceError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.ServiceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an AWS OpsWorks Stacks service error.
--
-- /See:/ 'newServiceError' smart constructor.
data ServiceError = ServiceError'
  { -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | A message that describes the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The error ID.
    serviceErrorId :: Prelude.Maybe Prelude.Text,
    -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | When the error occurred.
    createdAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'serviceError_stackId' - The stack ID.
--
-- 'message', 'serviceError_message' - A message that describes the error.
--
-- 'type'', 'serviceError_type' - The error type.
--
-- 'serviceErrorId', 'serviceError_serviceErrorId' - The error ID.
--
-- 'instanceId', 'serviceError_instanceId' - The instance ID.
--
-- 'createdAt', 'serviceError_createdAt' - When the error occurred.
newServiceError ::
  ServiceError
newServiceError =
  ServiceError'
    { stackId = Prelude.Nothing,
      message = Prelude.Nothing,
      type' = Prelude.Nothing,
      serviceErrorId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The stack ID.
serviceError_stackId :: Lens.Lens' ServiceError (Prelude.Maybe Prelude.Text)
serviceError_stackId = Lens.lens (\ServiceError' {stackId} -> stackId) (\s@ServiceError' {} a -> s {stackId = a} :: ServiceError)

-- | A message that describes the error.
serviceError_message :: Lens.Lens' ServiceError (Prelude.Maybe Prelude.Text)
serviceError_message = Lens.lens (\ServiceError' {message} -> message) (\s@ServiceError' {} a -> s {message = a} :: ServiceError)

-- | The error type.
serviceError_type :: Lens.Lens' ServiceError (Prelude.Maybe Prelude.Text)
serviceError_type = Lens.lens (\ServiceError' {type'} -> type') (\s@ServiceError' {} a -> s {type' = a} :: ServiceError)

-- | The error ID.
serviceError_serviceErrorId :: Lens.Lens' ServiceError (Prelude.Maybe Prelude.Text)
serviceError_serviceErrorId = Lens.lens (\ServiceError' {serviceErrorId} -> serviceErrorId) (\s@ServiceError' {} a -> s {serviceErrorId = a} :: ServiceError)

-- | The instance ID.
serviceError_instanceId :: Lens.Lens' ServiceError (Prelude.Maybe Prelude.Text)
serviceError_instanceId = Lens.lens (\ServiceError' {instanceId} -> instanceId) (\s@ServiceError' {} a -> s {instanceId = a} :: ServiceError)

-- | When the error occurred.
serviceError_createdAt :: Lens.Lens' ServiceError (Prelude.Maybe Prelude.Text)
serviceError_createdAt = Lens.lens (\ServiceError' {createdAt} -> createdAt) (\s@ServiceError' {} a -> s {createdAt = a} :: ServiceError)

instance Core.FromJSON ServiceError where
  parseJSON =
    Core.withObject
      "ServiceError"
      ( \x ->
          ServiceError'
            Prelude.<$> (x Core..:? "StackId")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ServiceErrorId")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable ServiceError where
  hashWithSalt _salt ServiceError' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` serviceErrorId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData ServiceError where
  rnf ServiceError' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf serviceErrorId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf createdAt
