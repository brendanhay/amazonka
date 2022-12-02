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
-- Module      : Amazonka.SageMakerEdge.Types.Checksum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.Checksum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.ChecksumType

-- | Information about the checksum of a model deployed on a device.
--
-- /See:/ 'newChecksum' smart constructor.
data Checksum = Checksum'
  { -- | The type of the checksum.
    type' :: Prelude.Maybe ChecksumType,
    -- | The checksum of the model.
    sum :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Checksum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'checksum_type' - The type of the checksum.
--
-- 'sum', 'checksum_sum' - The checksum of the model.
newChecksum ::
  Checksum
newChecksum =
  Checksum'
    { type' = Prelude.Nothing,
      sum = Prelude.Nothing
    }

-- | The type of the checksum.
checksum_type :: Lens.Lens' Checksum (Prelude.Maybe ChecksumType)
checksum_type = Lens.lens (\Checksum' {type'} -> type') (\s@Checksum' {} a -> s {type' = a} :: Checksum)

-- | The checksum of the model.
checksum_sum :: Lens.Lens' Checksum (Prelude.Maybe Prelude.Text)
checksum_sum = Lens.lens (\Checksum' {sum} -> sum) (\s@Checksum' {} a -> s {sum = a} :: Checksum)

instance Data.FromJSON Checksum where
  parseJSON =
    Data.withObject
      "Checksum"
      ( \x ->
          Checksum'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Sum")
      )

instance Prelude.Hashable Checksum where
  hashWithSalt _salt Checksum' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` sum

instance Prelude.NFData Checksum where
  rnf Checksum' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf sum
