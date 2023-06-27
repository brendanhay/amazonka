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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2InstanceMonitoringDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2InstanceMonitoringDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of monitoring that’s turned on for an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2InstanceMonitoringDetails' smart constructor.
data AwsEc2InstanceMonitoringDetails = AwsEc2InstanceMonitoringDetails'
  { -- | Indicates whether detailed monitoring is turned on. Otherwise, basic
    -- monitoring is turned on.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2InstanceMonitoringDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'awsEc2InstanceMonitoringDetails_state' - Indicates whether detailed monitoring is turned on. Otherwise, basic
-- monitoring is turned on.
newAwsEc2InstanceMonitoringDetails ::
  AwsEc2InstanceMonitoringDetails
newAwsEc2InstanceMonitoringDetails =
  AwsEc2InstanceMonitoringDetails'
    { state =
        Prelude.Nothing
    }

-- | Indicates whether detailed monitoring is turned on. Otherwise, basic
-- monitoring is turned on.
awsEc2InstanceMonitoringDetails_state :: Lens.Lens' AwsEc2InstanceMonitoringDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceMonitoringDetails_state = Lens.lens (\AwsEc2InstanceMonitoringDetails' {state} -> state) (\s@AwsEc2InstanceMonitoringDetails' {} a -> s {state = a} :: AwsEc2InstanceMonitoringDetails)

instance
  Data.FromJSON
    AwsEc2InstanceMonitoringDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2InstanceMonitoringDetails"
      ( \x ->
          AwsEc2InstanceMonitoringDetails'
            Prelude.<$> (x Data..:? "State")
      )

instance
  Prelude.Hashable
    AwsEc2InstanceMonitoringDetails
  where
  hashWithSalt
    _salt
    AwsEc2InstanceMonitoringDetails' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    AwsEc2InstanceMonitoringDetails
  where
  rnf AwsEc2InstanceMonitoringDetails' {..} =
    Prelude.rnf state

instance Data.ToJSON AwsEc2InstanceMonitoringDetails where
  toJSON AwsEc2InstanceMonitoringDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("State" Data..=) Prelude.<$> state]
      )
