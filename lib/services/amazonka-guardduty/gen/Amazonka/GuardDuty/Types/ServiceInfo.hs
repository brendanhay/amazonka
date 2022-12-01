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
-- Module      : Amazonka.GuardDuty.Types.ServiceInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ServiceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.Action
import Amazonka.GuardDuty.Types.EbsVolumeScanDetails
import Amazonka.GuardDuty.Types.Evidence
import Amazonka.GuardDuty.Types.ServiceAdditionalInfo
import qualified Amazonka.Prelude as Prelude

-- | Contains additional information about the generated finding.
--
-- /See:/ 'newServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | Contains additional information about the generated finding.
    additionalInfo :: Prelude.Maybe ServiceAdditionalInfo,
    -- | The resource role information for this finding.
    resourceRole :: Prelude.Maybe Prelude.Text,
    -- | An evidence object associated with the service.
    evidence :: Prelude.Maybe Evidence,
    -- | Feedback that was submitted about the finding.
    userFeedback :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature that generated a finding.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The total count of the occurrences of this finding type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The first-seen timestamp of the activity that prompted GuardDuty to
    -- generate this finding.
    eventFirstSeen :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this finding is archived.
    archived :: Prelude.Maybe Prelude.Bool,
    -- | Information about the activity that is described in a finding.
    action :: Prelude.Maybe Action,
    -- | Returns details from the malware scan that created a finding.
    ebsVolumeScanDetails :: Prelude.Maybe EbsVolumeScanDetails,
    -- | The detector ID for the GuardDuty service.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services service (GuardDuty) that generated a
    -- finding.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The last-seen timestamp of the activity that prompted GuardDuty to
    -- generate this finding.
    eventLastSeen :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'serviceInfo_additionalInfo' - Contains additional information about the generated finding.
--
-- 'resourceRole', 'serviceInfo_resourceRole' - The resource role information for this finding.
--
-- 'evidence', 'serviceInfo_evidence' - An evidence object associated with the service.
--
-- 'userFeedback', 'serviceInfo_userFeedback' - Feedback that was submitted about the finding.
--
-- 'featureName', 'serviceInfo_featureName' - The name of the feature that generated a finding.
--
-- 'count', 'serviceInfo_count' - The total count of the occurrences of this finding type.
--
-- 'eventFirstSeen', 'serviceInfo_eventFirstSeen' - The first-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
--
-- 'archived', 'serviceInfo_archived' - Indicates whether this finding is archived.
--
-- 'action', 'serviceInfo_action' - Information about the activity that is described in a finding.
--
-- 'ebsVolumeScanDetails', 'serviceInfo_ebsVolumeScanDetails' - Returns details from the malware scan that created a finding.
--
-- 'detectorId', 'serviceInfo_detectorId' - The detector ID for the GuardDuty service.
--
-- 'serviceName', 'serviceInfo_serviceName' - The name of the Amazon Web Services service (GuardDuty) that generated a
-- finding.
--
-- 'eventLastSeen', 'serviceInfo_eventLastSeen' - The last-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
newServiceInfo ::
  ServiceInfo
newServiceInfo =
  ServiceInfo'
    { additionalInfo = Prelude.Nothing,
      resourceRole = Prelude.Nothing,
      evidence = Prelude.Nothing,
      userFeedback = Prelude.Nothing,
      featureName = Prelude.Nothing,
      count = Prelude.Nothing,
      eventFirstSeen = Prelude.Nothing,
      archived = Prelude.Nothing,
      action = Prelude.Nothing,
      ebsVolumeScanDetails = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      eventLastSeen = Prelude.Nothing
    }

-- | Contains additional information about the generated finding.
serviceInfo_additionalInfo :: Lens.Lens' ServiceInfo (Prelude.Maybe ServiceAdditionalInfo)
serviceInfo_additionalInfo = Lens.lens (\ServiceInfo' {additionalInfo} -> additionalInfo) (\s@ServiceInfo' {} a -> s {additionalInfo = a} :: ServiceInfo)

-- | The resource role information for this finding.
serviceInfo_resourceRole :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_resourceRole = Lens.lens (\ServiceInfo' {resourceRole} -> resourceRole) (\s@ServiceInfo' {} a -> s {resourceRole = a} :: ServiceInfo)

-- | An evidence object associated with the service.
serviceInfo_evidence :: Lens.Lens' ServiceInfo (Prelude.Maybe Evidence)
serviceInfo_evidence = Lens.lens (\ServiceInfo' {evidence} -> evidence) (\s@ServiceInfo' {} a -> s {evidence = a} :: ServiceInfo)

-- | Feedback that was submitted about the finding.
serviceInfo_userFeedback :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_userFeedback = Lens.lens (\ServiceInfo' {userFeedback} -> userFeedback) (\s@ServiceInfo' {} a -> s {userFeedback = a} :: ServiceInfo)

-- | The name of the feature that generated a finding.
serviceInfo_featureName :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_featureName = Lens.lens (\ServiceInfo' {featureName} -> featureName) (\s@ServiceInfo' {} a -> s {featureName = a} :: ServiceInfo)

-- | The total count of the occurrences of this finding type.
serviceInfo_count :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Int)
serviceInfo_count = Lens.lens (\ServiceInfo' {count} -> count) (\s@ServiceInfo' {} a -> s {count = a} :: ServiceInfo)

-- | The first-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
serviceInfo_eventFirstSeen :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_eventFirstSeen = Lens.lens (\ServiceInfo' {eventFirstSeen} -> eventFirstSeen) (\s@ServiceInfo' {} a -> s {eventFirstSeen = a} :: ServiceInfo)

-- | Indicates whether this finding is archived.
serviceInfo_archived :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Bool)
serviceInfo_archived = Lens.lens (\ServiceInfo' {archived} -> archived) (\s@ServiceInfo' {} a -> s {archived = a} :: ServiceInfo)

-- | Information about the activity that is described in a finding.
serviceInfo_action :: Lens.Lens' ServiceInfo (Prelude.Maybe Action)
serviceInfo_action = Lens.lens (\ServiceInfo' {action} -> action) (\s@ServiceInfo' {} a -> s {action = a} :: ServiceInfo)

-- | Returns details from the malware scan that created a finding.
serviceInfo_ebsVolumeScanDetails :: Lens.Lens' ServiceInfo (Prelude.Maybe EbsVolumeScanDetails)
serviceInfo_ebsVolumeScanDetails = Lens.lens (\ServiceInfo' {ebsVolumeScanDetails} -> ebsVolumeScanDetails) (\s@ServiceInfo' {} a -> s {ebsVolumeScanDetails = a} :: ServiceInfo)

-- | The detector ID for the GuardDuty service.
serviceInfo_detectorId :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_detectorId = Lens.lens (\ServiceInfo' {detectorId} -> detectorId) (\s@ServiceInfo' {} a -> s {detectorId = a} :: ServiceInfo)

-- | The name of the Amazon Web Services service (GuardDuty) that generated a
-- finding.
serviceInfo_serviceName :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_serviceName = Lens.lens (\ServiceInfo' {serviceName} -> serviceName) (\s@ServiceInfo' {} a -> s {serviceName = a} :: ServiceInfo)

-- | The last-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
serviceInfo_eventLastSeen :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_eventLastSeen = Lens.lens (\ServiceInfo' {eventLastSeen} -> eventLastSeen) (\s@ServiceInfo' {} a -> s {eventLastSeen = a} :: ServiceInfo)

instance Core.FromJSON ServiceInfo where
  parseJSON =
    Core.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Prelude.<$> (x Core..:? "additionalInfo")
            Prelude.<*> (x Core..:? "resourceRole")
            Prelude.<*> (x Core..:? "evidence")
            Prelude.<*> (x Core..:? "userFeedback")
            Prelude.<*> (x Core..:? "featureName")
            Prelude.<*> (x Core..:? "count")
            Prelude.<*> (x Core..:? "eventFirstSeen")
            Prelude.<*> (x Core..:? "archived")
            Prelude.<*> (x Core..:? "action")
            Prelude.<*> (x Core..:? "ebsVolumeScanDetails")
            Prelude.<*> (x Core..:? "detectorId")
            Prelude.<*> (x Core..:? "serviceName")
            Prelude.<*> (x Core..:? "eventLastSeen")
      )

instance Prelude.Hashable ServiceInfo where
  hashWithSalt _salt ServiceInfo' {..} =
    _salt `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` resourceRole
      `Prelude.hashWithSalt` evidence
      `Prelude.hashWithSalt` userFeedback
      `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` eventFirstSeen
      `Prelude.hashWithSalt` archived
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` ebsVolumeScanDetails
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` eventLastSeen

instance Prelude.NFData ServiceInfo where
  rnf ServiceInfo' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf resourceRole
      `Prelude.seq` Prelude.rnf evidence
      `Prelude.seq` Prelude.rnf userFeedback
      `Prelude.seq` Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf eventFirstSeen
      `Prelude.seq` Prelude.rnf archived
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf ebsVolumeScanDetails
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf eventLastSeen
