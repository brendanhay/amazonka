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
-- Module      : Amazonka.DrS.Types.SourceNetworkData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.SourceNetworkData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties of Source Network related to a job event.
--
-- /See:/ 'newSourceNetworkData' smart constructor.
data SourceNetworkData = SourceNetworkData'
  { -- | Source Network ID.
    sourceNetworkID :: Prelude.Maybe Prelude.Text,
    -- | VPC ID protected by the Source Network.
    sourceVpc :: Prelude.Maybe Prelude.Text,
    -- | CloudFormation stack name that was deployed for recovering the Source
    -- Network.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | ID of the recovered VPC following Source Network recovery.
    targetVpc :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceNetworkData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'sourceNetworkData_sourceNetworkID' - Source Network ID.
--
-- 'sourceVpc', 'sourceNetworkData_sourceVpc' - VPC ID protected by the Source Network.
--
-- 'stackName', 'sourceNetworkData_stackName' - CloudFormation stack name that was deployed for recovering the Source
-- Network.
--
-- 'targetVpc', 'sourceNetworkData_targetVpc' - ID of the recovered VPC following Source Network recovery.
newSourceNetworkData ::
  SourceNetworkData
newSourceNetworkData =
  SourceNetworkData'
    { sourceNetworkID =
        Prelude.Nothing,
      sourceVpc = Prelude.Nothing,
      stackName = Prelude.Nothing,
      targetVpc = Prelude.Nothing
    }

-- | Source Network ID.
sourceNetworkData_sourceNetworkID :: Lens.Lens' SourceNetworkData (Prelude.Maybe Prelude.Text)
sourceNetworkData_sourceNetworkID = Lens.lens (\SourceNetworkData' {sourceNetworkID} -> sourceNetworkID) (\s@SourceNetworkData' {} a -> s {sourceNetworkID = a} :: SourceNetworkData)

-- | VPC ID protected by the Source Network.
sourceNetworkData_sourceVpc :: Lens.Lens' SourceNetworkData (Prelude.Maybe Prelude.Text)
sourceNetworkData_sourceVpc = Lens.lens (\SourceNetworkData' {sourceVpc} -> sourceVpc) (\s@SourceNetworkData' {} a -> s {sourceVpc = a} :: SourceNetworkData)

-- | CloudFormation stack name that was deployed for recovering the Source
-- Network.
sourceNetworkData_stackName :: Lens.Lens' SourceNetworkData (Prelude.Maybe Prelude.Text)
sourceNetworkData_stackName = Lens.lens (\SourceNetworkData' {stackName} -> stackName) (\s@SourceNetworkData' {} a -> s {stackName = a} :: SourceNetworkData)

-- | ID of the recovered VPC following Source Network recovery.
sourceNetworkData_targetVpc :: Lens.Lens' SourceNetworkData (Prelude.Maybe Prelude.Text)
sourceNetworkData_targetVpc = Lens.lens (\SourceNetworkData' {targetVpc} -> targetVpc) (\s@SourceNetworkData' {} a -> s {targetVpc = a} :: SourceNetworkData)

instance Data.FromJSON SourceNetworkData where
  parseJSON =
    Data.withObject
      "SourceNetworkData"
      ( \x ->
          SourceNetworkData'
            Prelude.<$> (x Data..:? "sourceNetworkID")
            Prelude.<*> (x Data..:? "sourceVpc")
            Prelude.<*> (x Data..:? "stackName")
            Prelude.<*> (x Data..:? "targetVpc")
      )

instance Prelude.Hashable SourceNetworkData where
  hashWithSalt _salt SourceNetworkData' {..} =
    _salt
      `Prelude.hashWithSalt` sourceNetworkID
      `Prelude.hashWithSalt` sourceVpc
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` targetVpc

instance Prelude.NFData SourceNetworkData where
  rnf SourceNetworkData' {..} =
    Prelude.rnf sourceNetworkID
      `Prelude.seq` Prelude.rnf sourceVpc
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf targetVpc
