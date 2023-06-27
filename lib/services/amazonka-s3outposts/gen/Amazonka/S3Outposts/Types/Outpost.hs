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
-- Module      : Amazonka.S3Outposts.Types.Outpost
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3Outposts.Types.Outpost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details for the Outpost object.
--
-- /See:/ 'newOutpost' smart constructor.
data Outpost = Outpost'
  { -- | The Amazon S3 capacity of the outpost in bytes.
    capacityInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the unique Amazon Resource Name (ARN) for the outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the unique identifier for the outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    -- | Returns the Amazon Web Services account ID of the outpost owner. Useful
    -- for comparing owned versus shared outposts.
    ownerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Outpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityInBytes', 'outpost_capacityInBytes' - The Amazon S3 capacity of the outpost in bytes.
--
-- 'outpostArn', 'outpost_outpostArn' - Specifies the unique Amazon Resource Name (ARN) for the outpost.
--
-- 'outpostId', 'outpost_outpostId' - Specifies the unique identifier for the outpost.
--
-- 'ownerId', 'outpost_ownerId' - Returns the Amazon Web Services account ID of the outpost owner. Useful
-- for comparing owned versus shared outposts.
newOutpost ::
  Outpost
newOutpost =
  Outpost'
    { capacityInBytes = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      outpostId = Prelude.Nothing,
      ownerId = Prelude.Nothing
    }

-- | The Amazon S3 capacity of the outpost in bytes.
outpost_capacityInBytes :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Integer)
outpost_capacityInBytes = Lens.lens (\Outpost' {capacityInBytes} -> capacityInBytes) (\s@Outpost' {} a -> s {capacityInBytes = a} :: Outpost)

-- | Specifies the unique Amazon Resource Name (ARN) for the outpost.
outpost_outpostArn :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_outpostArn = Lens.lens (\Outpost' {outpostArn} -> outpostArn) (\s@Outpost' {} a -> s {outpostArn = a} :: Outpost)

-- | Specifies the unique identifier for the outpost.
outpost_outpostId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_outpostId = Lens.lens (\Outpost' {outpostId} -> outpostId) (\s@Outpost' {} a -> s {outpostId = a} :: Outpost)

-- | Returns the Amazon Web Services account ID of the outpost owner. Useful
-- for comparing owned versus shared outposts.
outpost_ownerId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_ownerId = Lens.lens (\Outpost' {ownerId} -> ownerId) (\s@Outpost' {} a -> s {ownerId = a} :: Outpost)

instance Data.FromJSON Outpost where
  parseJSON =
    Data.withObject
      "Outpost"
      ( \x ->
          Outpost'
            Prelude.<$> (x Data..:? "CapacityInBytes")
            Prelude.<*> (x Data..:? "OutpostArn")
            Prelude.<*> (x Data..:? "OutpostId")
            Prelude.<*> (x Data..:? "OwnerId")
      )

instance Prelude.Hashable Outpost where
  hashWithSalt _salt Outpost' {..} =
    _salt
      `Prelude.hashWithSalt` capacityInBytes
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` ownerId

instance Prelude.NFData Outpost where
  rnf Outpost' {..} =
    Prelude.rnf capacityInBytes
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf ownerId
