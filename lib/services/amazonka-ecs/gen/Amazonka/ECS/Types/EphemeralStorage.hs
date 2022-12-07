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
-- Module      : Amazonka.ECS.Types.EphemeralStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.EphemeralStorage where

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
-- /See:/ 'newEphemeralStorage' smart constructor.
data EphemeralStorage = EphemeralStorage'
  { -- | The total amount, in GiB, of ephemeral storage to set for the task. The
    -- minimum supported value is @21@ GiB and the maximum supported value is
    -- @200@ GiB.
    sizeInGiB :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemeralStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGiB', 'ephemeralStorage_sizeInGiB' - The total amount, in GiB, of ephemeral storage to set for the task. The
-- minimum supported value is @21@ GiB and the maximum supported value is
-- @200@ GiB.
newEphemeralStorage ::
  -- | 'sizeInGiB'
  Prelude.Int ->
  EphemeralStorage
newEphemeralStorage pSizeInGiB_ =
  EphemeralStorage' {sizeInGiB = pSizeInGiB_}

-- | The total amount, in GiB, of ephemeral storage to set for the task. The
-- minimum supported value is @21@ GiB and the maximum supported value is
-- @200@ GiB.
ephemeralStorage_sizeInGiB :: Lens.Lens' EphemeralStorage Prelude.Int
ephemeralStorage_sizeInGiB = Lens.lens (\EphemeralStorage' {sizeInGiB} -> sizeInGiB) (\s@EphemeralStorage' {} a -> s {sizeInGiB = a} :: EphemeralStorage)

instance Data.FromJSON EphemeralStorage where
  parseJSON =
    Data.withObject
      "EphemeralStorage"
      ( \x ->
          EphemeralStorage'
            Prelude.<$> (x Data..: "sizeInGiB")
      )

instance Prelude.Hashable EphemeralStorage where
  hashWithSalt _salt EphemeralStorage' {..} =
    _salt `Prelude.hashWithSalt` sizeInGiB

instance Prelude.NFData EphemeralStorage where
  rnf EphemeralStorage' {..} = Prelude.rnf sizeInGiB

instance Data.ToJSON EphemeralStorage where
  toJSON EphemeralStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("sizeInGiB" Data..= sizeInGiB)]
      )
