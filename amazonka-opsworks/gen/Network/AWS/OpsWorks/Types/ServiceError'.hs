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
-- Module      : Network.AWS.OpsWorks.Types.ServiceError'
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ServiceError' where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an AWS OpsWorks Stacks service error.
--
-- /See:/ 'newServiceError'' smart constructor.
data ServiceError' = ServiceError''
  { -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | A message that describes the error.
    message :: Core.Maybe Core.Text,
    -- | The error ID.
    serviceErrorId :: Core.Maybe Core.Text,
    -- | When the error occurred.
    createdAt :: Core.Maybe Core.Text,
    -- | The error type.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceError'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'serviceError'_instanceId' - The instance ID.
--
-- 'stackId', 'serviceError'_stackId' - The stack ID.
--
-- 'message', 'serviceError'_message' - A message that describes the error.
--
-- 'serviceErrorId', 'serviceError'_serviceErrorId' - The error ID.
--
-- 'createdAt', 'serviceError'_createdAt' - When the error occurred.
--
-- 'type'', 'serviceError'_type' - The error type.
newServiceError' ::
  ServiceError'
newServiceError' =
  ServiceError''
    { instanceId = Core.Nothing,
      stackId = Core.Nothing,
      message = Core.Nothing,
      serviceErrorId = Core.Nothing,
      createdAt = Core.Nothing,
      type' = Core.Nothing
    }

-- | The instance ID.
serviceError'_instanceId :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
serviceError'_instanceId = Lens.lens (\ServiceError'' {instanceId} -> instanceId) (\s@ServiceError'' {} a -> s {instanceId = a} :: ServiceError')

-- | The stack ID.
serviceError'_stackId :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
serviceError'_stackId = Lens.lens (\ServiceError'' {stackId} -> stackId) (\s@ServiceError'' {} a -> s {stackId = a} :: ServiceError')

-- | A message that describes the error.
serviceError'_message :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
serviceError'_message = Lens.lens (\ServiceError'' {message} -> message) (\s@ServiceError'' {} a -> s {message = a} :: ServiceError')

-- | The error ID.
serviceError'_serviceErrorId :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
serviceError'_serviceErrorId = Lens.lens (\ServiceError'' {serviceErrorId} -> serviceErrorId) (\s@ServiceError'' {} a -> s {serviceErrorId = a} :: ServiceError')

-- | When the error occurred.
serviceError'_createdAt :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
serviceError'_createdAt = Lens.lens (\ServiceError'' {createdAt} -> createdAt) (\s@ServiceError'' {} a -> s {createdAt = a} :: ServiceError')

-- | The error type.
serviceError'_type :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
serviceError'_type = Lens.lens (\ServiceError'' {type'} -> type') (\s@ServiceError'' {} a -> s {type' = a} :: ServiceError')

instance Core.FromJSON ServiceError' where
  parseJSON =
    Core.withObject
      "ServiceError'"
      ( \x ->
          ServiceError''
            Core.<$> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "ServiceErrorId")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ServiceError'

instance Core.NFData ServiceError'
