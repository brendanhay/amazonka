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
-- Module      : Amazonka.SSMIncidents.Types.AddRegionAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.AddRegionAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the Amazon Web Services Region and KMS key to add to the
-- replication set.
--
-- /See:/ 'newAddRegionAction' smart constructor.
data AddRegionAction = AddRegionAction'
  { -- | The KMS key ID to use to encrypt your replication set.
    sseKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region name to add to the replication set.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddRegionAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sseKmsKeyId', 'addRegionAction_sseKmsKeyId' - The KMS key ID to use to encrypt your replication set.
--
-- 'regionName', 'addRegionAction_regionName' - The Amazon Web Services Region name to add to the replication set.
newAddRegionAction ::
  -- | 'regionName'
  Prelude.Text ->
  AddRegionAction
newAddRegionAction pRegionName_ =
  AddRegionAction'
    { sseKmsKeyId = Prelude.Nothing,
      regionName = pRegionName_
    }

-- | The KMS key ID to use to encrypt your replication set.
addRegionAction_sseKmsKeyId :: Lens.Lens' AddRegionAction (Prelude.Maybe Prelude.Text)
addRegionAction_sseKmsKeyId = Lens.lens (\AddRegionAction' {sseKmsKeyId} -> sseKmsKeyId) (\s@AddRegionAction' {} a -> s {sseKmsKeyId = a} :: AddRegionAction)

-- | The Amazon Web Services Region name to add to the replication set.
addRegionAction_regionName :: Lens.Lens' AddRegionAction Prelude.Text
addRegionAction_regionName = Lens.lens (\AddRegionAction' {regionName} -> regionName) (\s@AddRegionAction' {} a -> s {regionName = a} :: AddRegionAction)

instance Prelude.Hashable AddRegionAction where
  hashWithSalt _salt AddRegionAction' {..} =
    _salt
      `Prelude.hashWithSalt` sseKmsKeyId
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData AddRegionAction where
  rnf AddRegionAction' {..} =
    Prelude.rnf sseKmsKeyId `Prelude.seq`
      Prelude.rnf regionName

instance Data.ToJSON AddRegionAction where
  toJSON AddRegionAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sseKmsKeyId" Data..=) Prelude.<$> sseKmsKeyId,
            Prelude.Just ("regionName" Data..= regionName)
          ]
      )
