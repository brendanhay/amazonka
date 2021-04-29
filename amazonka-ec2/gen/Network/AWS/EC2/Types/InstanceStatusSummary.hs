{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.InstanceStatusSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusSummary where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceStatusDetails
import Network.AWS.EC2.Types.SummaryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the status of an instance.
--
-- /See:/ 'newInstanceStatusSummary' smart constructor.
data InstanceStatusSummary = InstanceStatusSummary'
  { -- | The system instance health or application instance health.
    details :: Prelude.Maybe [InstanceStatusDetails],
    -- | The status.
    status :: SummaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
instanceStatusSummary_details = Lens.lens (\InstanceStatusSummary' {details} -> details) (\s@InstanceStatusSummary' {} a -> s {details = a} :: InstanceStatusSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The status.
instanceStatusSummary_status :: Lens.Lens' InstanceStatusSummary SummaryStatus
instanceStatusSummary_status = Lens.lens (\InstanceStatusSummary' {status} -> status) (\s@InstanceStatusSummary' {} a -> s {status = a} :: InstanceStatusSummary)

instance Prelude.FromXML InstanceStatusSummary where
  parseXML x =
    InstanceStatusSummary'
      Prelude.<$> ( x Prelude..@? "details" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@ "status")

instance Prelude.Hashable InstanceStatusSummary

instance Prelude.NFData InstanceStatusSummary
