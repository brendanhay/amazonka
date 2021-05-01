{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an AWS OpsWorks Stacks service error.
--
-- /See:/ 'newServiceError'' smart constructor.
data ServiceError' = ServiceError''
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | A message that describes the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error ID.
    serviceErrorId :: Prelude.Maybe Prelude.Text,
    -- | When the error occurred.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The error type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceId = Prelude.Nothing,
      stackId = Prelude.Nothing,
      message = Prelude.Nothing,
      serviceErrorId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The instance ID.
serviceError'_instanceId :: Lens.Lens' ServiceError' (Prelude.Maybe Prelude.Text)
serviceError'_instanceId = Lens.lens (\ServiceError'' {instanceId} -> instanceId) (\s@ServiceError'' {} a -> s {instanceId = a} :: ServiceError')

-- | The stack ID.
serviceError'_stackId :: Lens.Lens' ServiceError' (Prelude.Maybe Prelude.Text)
serviceError'_stackId = Lens.lens (\ServiceError'' {stackId} -> stackId) (\s@ServiceError'' {} a -> s {stackId = a} :: ServiceError')

-- | A message that describes the error.
serviceError'_message :: Lens.Lens' ServiceError' (Prelude.Maybe Prelude.Text)
serviceError'_message = Lens.lens (\ServiceError'' {message} -> message) (\s@ServiceError'' {} a -> s {message = a} :: ServiceError')

-- | The error ID.
serviceError'_serviceErrorId :: Lens.Lens' ServiceError' (Prelude.Maybe Prelude.Text)
serviceError'_serviceErrorId = Lens.lens (\ServiceError'' {serviceErrorId} -> serviceErrorId) (\s@ServiceError'' {} a -> s {serviceErrorId = a} :: ServiceError')

-- | When the error occurred.
serviceError'_createdAt :: Lens.Lens' ServiceError' (Prelude.Maybe Prelude.Text)
serviceError'_createdAt = Lens.lens (\ServiceError'' {createdAt} -> createdAt) (\s@ServiceError'' {} a -> s {createdAt = a} :: ServiceError')

-- | The error type.
serviceError'_type :: Lens.Lens' ServiceError' (Prelude.Maybe Prelude.Text)
serviceError'_type = Lens.lens (\ServiceError'' {type'} -> type') (\s@ServiceError'' {} a -> s {type' = a} :: ServiceError')

instance Prelude.FromJSON ServiceError' where
  parseJSON =
    Prelude.withObject
      "ServiceError'"
      ( \x ->
          ServiceError''
            Prelude.<$> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "StackId")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "ServiceErrorId")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable ServiceError'

instance Prelude.NFData ServiceError'
