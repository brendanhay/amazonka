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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ServiceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.Action
import Amazonka.GuardDuty.Types.EbsVolumeScanDetails
import Amazonka.GuardDuty.Types.Evidence
import Amazonka.GuardDuty.Types.ServiceAdditionalInfo
import qualified Amazonka.Prelude as Prelude

-- | Contains additional information about the generated finding.
--
-- /See:/ 'newServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | Information about the activity that is described in a finding.
    action :: Prelude.Maybe Action,
    -- | Contains additional information about the generated finding.
    additionalInfo :: Prelude.Maybe ServiceAdditionalInfo,
    -- | Indicates whether this finding is archived.
    archived :: Prelude.Maybe Prelude.Bool,
    -- | The total count of the occurrences of this finding type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The detector ID for the GuardDuty service.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | Returns details from the malware scan that created a finding.
    ebsVolumeScanDetails :: Prelude.Maybe EbsVolumeScanDetails,
    -- | The first-seen timestamp of the activity that prompted GuardDuty to
    -- generate this finding.
    eventFirstSeen :: Prelude.Maybe Prelude.Text,
    -- | The last-seen timestamp of the activity that prompted GuardDuty to
    -- generate this finding.
    eventLastSeen :: Prelude.Maybe Prelude.Text,
    -- | An evidence object associated with the service.
    evidence :: Prelude.Maybe Evidence,
    -- | The name of the feature that generated a finding.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The resource role information for this finding.
    resourceRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services service (GuardDuty) that generated a
    -- finding.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Feedback that was submitted about the finding.
    userFeedback :: Prelude.Maybe Prelude.Text
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
-- 'action', 'serviceInfo_action' - Information about the activity that is described in a finding.
--
-- 'additionalInfo', 'serviceInfo_additionalInfo' - Contains additional information about the generated finding.
--
-- 'archived', 'serviceInfo_archived' - Indicates whether this finding is archived.
--
-- 'count', 'serviceInfo_count' - The total count of the occurrences of this finding type.
--
-- 'detectorId', 'serviceInfo_detectorId' - The detector ID for the GuardDuty service.
--
-- 'ebsVolumeScanDetails', 'serviceInfo_ebsVolumeScanDetails' - Returns details from the malware scan that created a finding.
--
-- 'eventFirstSeen', 'serviceInfo_eventFirstSeen' - The first-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
--
-- 'eventLastSeen', 'serviceInfo_eventLastSeen' - The last-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
--
-- 'evidence', 'serviceInfo_evidence' - An evidence object associated with the service.
--
-- 'featureName', 'serviceInfo_featureName' - The name of the feature that generated a finding.
--
-- 'resourceRole', 'serviceInfo_resourceRole' - The resource role information for this finding.
--
-- 'serviceName', 'serviceInfo_serviceName' - The name of the Amazon Web Services service (GuardDuty) that generated a
-- finding.
--
-- 'userFeedback', 'serviceInfo_userFeedback' - Feedback that was submitted about the finding.
newServiceInfo ::
  ServiceInfo
newServiceInfo =
  ServiceInfo'
    { action = Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      archived = Prelude.Nothing,
      count = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      ebsVolumeScanDetails = Prelude.Nothing,
      eventFirstSeen = Prelude.Nothing,
      eventLastSeen = Prelude.Nothing,
      evidence = Prelude.Nothing,
      featureName = Prelude.Nothing,
      resourceRole = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      userFeedback = Prelude.Nothing
    }

-- | Information about the activity that is described in a finding.
serviceInfo_action :: Lens.Lens' ServiceInfo (Prelude.Maybe Action)
serviceInfo_action = Lens.lens (\ServiceInfo' {action} -> action) (\s@ServiceInfo' {} a -> s {action = a} :: ServiceInfo)

-- | Contains additional information about the generated finding.
serviceInfo_additionalInfo :: Lens.Lens' ServiceInfo (Prelude.Maybe ServiceAdditionalInfo)
serviceInfo_additionalInfo = Lens.lens (\ServiceInfo' {additionalInfo} -> additionalInfo) (\s@ServiceInfo' {} a -> s {additionalInfo = a} :: ServiceInfo)

-- | Indicates whether this finding is archived.
serviceInfo_archived :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Bool)
serviceInfo_archived = Lens.lens (\ServiceInfo' {archived} -> archived) (\s@ServiceInfo' {} a -> s {archived = a} :: ServiceInfo)

-- | The total count of the occurrences of this finding type.
serviceInfo_count :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Int)
serviceInfo_count = Lens.lens (\ServiceInfo' {count} -> count) (\s@ServiceInfo' {} a -> s {count = a} :: ServiceInfo)

-- | The detector ID for the GuardDuty service.
serviceInfo_detectorId :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_detectorId = Lens.lens (\ServiceInfo' {detectorId} -> detectorId) (\s@ServiceInfo' {} a -> s {detectorId = a} :: ServiceInfo)

-- | Returns details from the malware scan that created a finding.
serviceInfo_ebsVolumeScanDetails :: Lens.Lens' ServiceInfo (Prelude.Maybe EbsVolumeScanDetails)
serviceInfo_ebsVolumeScanDetails = Lens.lens (\ServiceInfo' {ebsVolumeScanDetails} -> ebsVolumeScanDetails) (\s@ServiceInfo' {} a -> s {ebsVolumeScanDetails = a} :: ServiceInfo)

-- | The first-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
serviceInfo_eventFirstSeen :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_eventFirstSeen = Lens.lens (\ServiceInfo' {eventFirstSeen} -> eventFirstSeen) (\s@ServiceInfo' {} a -> s {eventFirstSeen = a} :: ServiceInfo)

-- | The last-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
serviceInfo_eventLastSeen :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_eventLastSeen = Lens.lens (\ServiceInfo' {eventLastSeen} -> eventLastSeen) (\s@ServiceInfo' {} a -> s {eventLastSeen = a} :: ServiceInfo)

-- | An evidence object associated with the service.
serviceInfo_evidence :: Lens.Lens' ServiceInfo (Prelude.Maybe Evidence)
serviceInfo_evidence = Lens.lens (\ServiceInfo' {evidence} -> evidence) (\s@ServiceInfo' {} a -> s {evidence = a} :: ServiceInfo)

-- | The name of the feature that generated a finding.
serviceInfo_featureName :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_featureName = Lens.lens (\ServiceInfo' {featureName} -> featureName) (\s@ServiceInfo' {} a -> s {featureName = a} :: ServiceInfo)

-- | The resource role information for this finding.
serviceInfo_resourceRole :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_resourceRole = Lens.lens (\ServiceInfo' {resourceRole} -> resourceRole) (\s@ServiceInfo' {} a -> s {resourceRole = a} :: ServiceInfo)

-- | The name of the Amazon Web Services service (GuardDuty) that generated a
-- finding.
serviceInfo_serviceName :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_serviceName = Lens.lens (\ServiceInfo' {serviceName} -> serviceName) (\s@ServiceInfo' {} a -> s {serviceName = a} :: ServiceInfo)

-- | Feedback that was submitted about the finding.
serviceInfo_userFeedback :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_userFeedback = Lens.lens (\ServiceInfo' {userFeedback} -> userFeedback) (\s@ServiceInfo' {} a -> s {userFeedback = a} :: ServiceInfo)

instance Data.FromJSON ServiceInfo where
  parseJSON =
    Data.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "additionalInfo")
            Prelude.<*> (x Data..:? "archived")
            Prelude.<*> (x Data..:? "count")
            Prelude.<*> (x Data..:? "detectorId")
            Prelude.<*> (x Data..:? "ebsVolumeScanDetails")
            Prelude.<*> (x Data..:? "eventFirstSeen")
            Prelude.<*> (x Data..:? "eventLastSeen")
            Prelude.<*> (x Data..:? "evidence")
            Prelude.<*> (x Data..:? "featureName")
            Prelude.<*> (x Data..:? "resourceRole")
            Prelude.<*> (x Data..:? "serviceName")
            Prelude.<*> (x Data..:? "userFeedback")
      )

instance Prelude.Hashable ServiceInfo where
  hashWithSalt _salt ServiceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` archived
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` ebsVolumeScanDetails
      `Prelude.hashWithSalt` eventFirstSeen
      `Prelude.hashWithSalt` eventLastSeen
      `Prelude.hashWithSalt` evidence
      `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` resourceRole
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` userFeedback

instance Prelude.NFData ServiceInfo where
  rnf ServiceInfo' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf archived
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf ebsVolumeScanDetails
      `Prelude.seq` Prelude.rnf eventFirstSeen
      `Prelude.seq` Prelude.rnf eventLastSeen
      `Prelude.seq` Prelude.rnf evidence
      `Prelude.seq` Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf resourceRole
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf userFeedback
