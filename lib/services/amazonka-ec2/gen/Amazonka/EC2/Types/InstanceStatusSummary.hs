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
-- Module      : Amazonka.EC2.Types.InstanceStatusSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStatusSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceStatusDetails
import Amazonka.EC2.Types.SummaryStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of an instance.
--
-- /See:/ 'newInstanceStatusSummary' smart constructor.
data InstanceStatusSummary = InstanceStatusSummary'
  { -- | The system instance health or application instance health.
    details :: Prelude.Maybe [InstanceStatusDetails],
    -- | The status.
    status :: SummaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStatusSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'instanceStatusSummary_details' - The system instance health or application instance health.
--
-- 'status', 'instanceStatusSummary_status' - The status.
newInstanceStatusSummary ::
  -- | 'status'
  SummaryStatus ->
  InstanceStatusSummary
newInstanceStatusSummary pStatus_ =
  InstanceStatusSummary'
    { details = Prelude.Nothing,
      status = pStatus_
    }

-- | The system instance health or application instance health.
instanceStatusSummary_details :: Lens.Lens' InstanceStatusSummary (Prelude.Maybe [InstanceStatusDetails])
instanceStatusSummary_details = Lens.lens (\InstanceStatusSummary' {details} -> details) (\s@InstanceStatusSummary' {} a -> s {details = a} :: InstanceStatusSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status.
instanceStatusSummary_status :: Lens.Lens' InstanceStatusSummary SummaryStatus
instanceStatusSummary_status = Lens.lens (\InstanceStatusSummary' {status} -> status) (\s@InstanceStatusSummary' {} a -> s {status = a} :: InstanceStatusSummary)

instance Data.FromXML InstanceStatusSummary where
  parseXML x =
    InstanceStatusSummary'
      Prelude.<$> ( x Data..@? "details" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@ "status")

instance Prelude.Hashable InstanceStatusSummary where
  hashWithSalt _salt InstanceStatusSummary' {..} =
    _salt `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` status

instance Prelude.NFData InstanceStatusSummary where
  rnf InstanceStatusSummary' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf status
