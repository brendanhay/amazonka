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
-- Module      : Amazonka.SSMIncidents.Types.RegionMapInputValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.RegionMapInputValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The mapping between a Amazon Web Services Region and the key that\'s
-- used to encrypt the data.
--
-- /See:/ 'newRegionMapInputValue' smart constructor.
data RegionMapInputValue = RegionMapInputValue'
  { -- | The KMS key used to encrypt the data in your replication set.
    sseKmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionMapInputValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sseKmsKeyId', 'regionMapInputValue_sseKmsKeyId' - The KMS key used to encrypt the data in your replication set.
newRegionMapInputValue ::
  RegionMapInputValue
newRegionMapInputValue =
  RegionMapInputValue' {sseKmsKeyId = Prelude.Nothing}

-- | The KMS key used to encrypt the data in your replication set.
regionMapInputValue_sseKmsKeyId :: Lens.Lens' RegionMapInputValue (Prelude.Maybe Prelude.Text)
regionMapInputValue_sseKmsKeyId = Lens.lens (\RegionMapInputValue' {sseKmsKeyId} -> sseKmsKeyId) (\s@RegionMapInputValue' {} a -> s {sseKmsKeyId = a} :: RegionMapInputValue)

instance Prelude.Hashable RegionMapInputValue where
  hashWithSalt _salt RegionMapInputValue' {..} =
    _salt `Prelude.hashWithSalt` sseKmsKeyId

instance Prelude.NFData RegionMapInputValue where
  rnf RegionMapInputValue' {..} =
    Prelude.rnf sseKmsKeyId

instance Data.ToJSON RegionMapInputValue where
  toJSON RegionMapInputValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [("sseKmsKeyId" Data..=) Prelude.<$> sseKmsKeyId]
      )
