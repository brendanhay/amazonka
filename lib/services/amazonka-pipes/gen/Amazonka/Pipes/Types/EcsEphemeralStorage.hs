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
-- Module      : Amazonka.Pipes.Types.EcsEphemeralStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsEphemeralStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amount of ephemeral storage to allocate for the task. This parameter
-- is used to expand the total amount of ephemeral storage available,
-- beyond the default amount, for tasks hosted on Fargate. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/using_data_volumes.html Fargate task storage>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- This parameter is only supported for tasks hosted on Fargate using Linux
-- platform version @1.4.0@ or later. This parameter is not supported for
-- Windows containers on Fargate.
--
-- /See:/ 'newEcsEphemeralStorage' smart constructor.
data EcsEphemeralStorage = EcsEphemeralStorage'
  { -- | The total amount, in GiB, of ephemeral storage to set for the task. The
    -- minimum supported value is @21@ GiB and the maximum supported value is
    -- @200@ GiB.
    sizeInGiB :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsEphemeralStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGiB', 'ecsEphemeralStorage_sizeInGiB' - The total amount, in GiB, of ephemeral storage to set for the task. The
-- minimum supported value is @21@ GiB and the maximum supported value is
-- @200@ GiB.
newEcsEphemeralStorage ::
  -- | 'sizeInGiB'
  Prelude.Natural ->
  EcsEphemeralStorage
newEcsEphemeralStorage pSizeInGiB_ =
  EcsEphemeralStorage' {sizeInGiB = pSizeInGiB_}

-- | The total amount, in GiB, of ephemeral storage to set for the task. The
-- minimum supported value is @21@ GiB and the maximum supported value is
-- @200@ GiB.
ecsEphemeralStorage_sizeInGiB :: Lens.Lens' EcsEphemeralStorage Prelude.Natural
ecsEphemeralStorage_sizeInGiB = Lens.lens (\EcsEphemeralStorage' {sizeInGiB} -> sizeInGiB) (\s@EcsEphemeralStorage' {} a -> s {sizeInGiB = a} :: EcsEphemeralStorage)

instance Data.FromJSON EcsEphemeralStorage where
  parseJSON =
    Data.withObject
      "EcsEphemeralStorage"
      ( \x ->
          EcsEphemeralStorage'
            Prelude.<$> (x Data..: "sizeInGiB")
      )

instance Prelude.Hashable EcsEphemeralStorage where
  hashWithSalt _salt EcsEphemeralStorage' {..} =
    _salt `Prelude.hashWithSalt` sizeInGiB

instance Prelude.NFData EcsEphemeralStorage where
  rnf EcsEphemeralStorage' {..} = Prelude.rnf sizeInGiB

instance Data.ToJSON EcsEphemeralStorage where
  toJSON EcsEphemeralStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("sizeInGiB" Data..= sizeInGiB)]
      )
