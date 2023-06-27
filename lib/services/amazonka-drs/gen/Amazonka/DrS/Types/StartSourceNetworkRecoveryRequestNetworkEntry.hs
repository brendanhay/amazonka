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
-- Module      : Amazonka.DrS.Types.StartSourceNetworkRecoveryRequestNetworkEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.StartSourceNetworkRecoveryRequestNetworkEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the Source Network to recover.
--
-- /See:/ 'newStartSourceNetworkRecoveryRequestNetworkEntry' smart constructor.
data StartSourceNetworkRecoveryRequestNetworkEntry = StartSourceNetworkRecoveryRequestNetworkEntry'
  { -- | CloudFormation stack name to be used for recovering the network.
    cfnStackName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the Source Network you want to recover.
    sourceNetworkID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSourceNetworkRecoveryRequestNetworkEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cfnStackName', 'startSourceNetworkRecoveryRequestNetworkEntry_cfnStackName' - CloudFormation stack name to be used for recovering the network.
--
-- 'sourceNetworkID', 'startSourceNetworkRecoveryRequestNetworkEntry_sourceNetworkID' - The ID of the Source Network you want to recover.
newStartSourceNetworkRecoveryRequestNetworkEntry ::
  -- | 'sourceNetworkID'
  Prelude.Text ->
  StartSourceNetworkRecoveryRequestNetworkEntry
newStartSourceNetworkRecoveryRequestNetworkEntry
  pSourceNetworkID_ =
    StartSourceNetworkRecoveryRequestNetworkEntry'
      { cfnStackName =
          Prelude.Nothing,
        sourceNetworkID =
          pSourceNetworkID_
      }

-- | CloudFormation stack name to be used for recovering the network.
startSourceNetworkRecoveryRequestNetworkEntry_cfnStackName :: Lens.Lens' StartSourceNetworkRecoveryRequestNetworkEntry (Prelude.Maybe Prelude.Text)
startSourceNetworkRecoveryRequestNetworkEntry_cfnStackName = Lens.lens (\StartSourceNetworkRecoveryRequestNetworkEntry' {cfnStackName} -> cfnStackName) (\s@StartSourceNetworkRecoveryRequestNetworkEntry' {} a -> s {cfnStackName = a} :: StartSourceNetworkRecoveryRequestNetworkEntry) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the Source Network you want to recover.
startSourceNetworkRecoveryRequestNetworkEntry_sourceNetworkID :: Lens.Lens' StartSourceNetworkRecoveryRequestNetworkEntry Prelude.Text
startSourceNetworkRecoveryRequestNetworkEntry_sourceNetworkID = Lens.lens (\StartSourceNetworkRecoveryRequestNetworkEntry' {sourceNetworkID} -> sourceNetworkID) (\s@StartSourceNetworkRecoveryRequestNetworkEntry' {} a -> s {sourceNetworkID = a} :: StartSourceNetworkRecoveryRequestNetworkEntry)

instance
  Prelude.Hashable
    StartSourceNetworkRecoveryRequestNetworkEntry
  where
  hashWithSalt
    _salt
    StartSourceNetworkRecoveryRequestNetworkEntry' {..} =
      _salt
        `Prelude.hashWithSalt` cfnStackName
        `Prelude.hashWithSalt` sourceNetworkID

instance
  Prelude.NFData
    StartSourceNetworkRecoveryRequestNetworkEntry
  where
  rnf
    StartSourceNetworkRecoveryRequestNetworkEntry' {..} =
      Prelude.rnf cfnStackName
        `Prelude.seq` Prelude.rnf sourceNetworkID

instance
  Data.ToJSON
    StartSourceNetworkRecoveryRequestNetworkEntry
  where
  toJSON
    StartSourceNetworkRecoveryRequestNetworkEntry' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("cfnStackName" Data..=) Prelude.<$> cfnStackName,
              Prelude.Just
                ("sourceNetworkID" Data..= sourceNetworkID)
            ]
        )
