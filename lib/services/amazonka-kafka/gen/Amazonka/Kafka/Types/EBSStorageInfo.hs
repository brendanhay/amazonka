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
-- Module      : Amazonka.Kafka.Types.EBSStorageInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.EBSStorageInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the EBS storage volumes attached to Kafka
-- broker nodes.
--
-- /See:/ 'newEBSStorageInfo' smart constructor.
data EBSStorageInfo = EBSStorageInfo'
  { -- | The size in GiB of the EBS volume for the data drive on each broker
    -- node.
    volumeSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSStorageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeSize', 'eBSStorageInfo_volumeSize' - The size in GiB of the EBS volume for the data drive on each broker
-- node.
newEBSStorageInfo ::
  EBSStorageInfo
newEBSStorageInfo =
  EBSStorageInfo' {volumeSize = Prelude.Nothing}

-- | The size in GiB of the EBS volume for the data drive on each broker
-- node.
eBSStorageInfo_volumeSize :: Lens.Lens' EBSStorageInfo (Prelude.Maybe Prelude.Natural)
eBSStorageInfo_volumeSize = Lens.lens (\EBSStorageInfo' {volumeSize} -> volumeSize) (\s@EBSStorageInfo' {} a -> s {volumeSize = a} :: EBSStorageInfo)

instance Core.FromJSON EBSStorageInfo where
  parseJSON =
    Core.withObject
      "EBSStorageInfo"
      ( \x ->
          EBSStorageInfo'
            Prelude.<$> (x Core..:? "volumeSize")
      )

instance Prelude.Hashable EBSStorageInfo where
  hashWithSalt salt' EBSStorageInfo' {..} =
    salt' `Prelude.hashWithSalt` volumeSize

instance Prelude.NFData EBSStorageInfo where
  rnf EBSStorageInfo' {..} = Prelude.rnf volumeSize

instance Core.ToJSON EBSStorageInfo where
  toJSON EBSStorageInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [("volumeSize" Core..=) Prelude.<$> volumeSize]
      )
