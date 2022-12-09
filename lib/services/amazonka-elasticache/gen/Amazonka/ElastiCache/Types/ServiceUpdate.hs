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
-- Module      : Amazonka.ElastiCache.Types.ServiceUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ServiceUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.ServiceUpdateSeverity
import Amazonka.ElastiCache.Types.ServiceUpdateStatus
import Amazonka.ElastiCache.Types.ServiceUpdateType
import qualified Amazonka.Prelude as Prelude

-- | An update that you can apply to your Redis clusters.
--
-- /See:/ 'newServiceUpdate' smart constructor.
data ServiceUpdate = ServiceUpdate'
  { -- | Indicates whether the service update will be automatically applied once
    -- the recommended apply-by date has expired.
    autoUpdateAfterRecommendedApplyByDate :: Prelude.Maybe Prelude.Bool,
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Prelude.Maybe Prelude.Text,
    -- | The Elasticache engine version to which the update applies. Either Redis
    -- or Memcached engine version
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The estimated length of time the service update will take
    estimatedUpdateTime :: Prelude.Maybe Prelude.Text,
    -- | Provides details of the service update
    serviceUpdateDescription :: Prelude.Maybe Prelude.Text,
    -- | The date after which the service update is no longer available
    serviceUpdateEndDate :: Prelude.Maybe Data.ISO8601,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The recommendend date to apply the service update in order to ensure
    -- compliance. For information on compliance, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
    serviceUpdateRecommendedApplyByDate :: Prelude.Maybe Data.ISO8601,
    -- | The date when the service update is initially available
    serviceUpdateReleaseDate :: Prelude.Maybe Data.ISO8601,
    -- | The severity of the service update
    serviceUpdateSeverity :: Prelude.Maybe ServiceUpdateSeverity,
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe ServiceUpdateStatus,
    -- | Reflects the nature of the service update
    serviceUpdateType :: Prelude.Maybe ServiceUpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoUpdateAfterRecommendedApplyByDate', 'serviceUpdate_autoUpdateAfterRecommendedApplyByDate' - Indicates whether the service update will be automatically applied once
-- the recommended apply-by date has expired.
--
-- 'engine', 'serviceUpdate_engine' - The Elasticache engine to which the update applies. Either Redis or
-- Memcached
--
-- 'engineVersion', 'serviceUpdate_engineVersion' - The Elasticache engine version to which the update applies. Either Redis
-- or Memcached engine version
--
-- 'estimatedUpdateTime', 'serviceUpdate_estimatedUpdateTime' - The estimated length of time the service update will take
--
-- 'serviceUpdateDescription', 'serviceUpdate_serviceUpdateDescription' - Provides details of the service update
--
-- 'serviceUpdateEndDate', 'serviceUpdate_serviceUpdateEndDate' - The date after which the service update is no longer available
--
-- 'serviceUpdateName', 'serviceUpdate_serviceUpdateName' - The unique ID of the service update
--
-- 'serviceUpdateRecommendedApplyByDate', 'serviceUpdate_serviceUpdateRecommendedApplyByDate' - The recommendend date to apply the service update in order to ensure
-- compliance. For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
--
-- 'serviceUpdateReleaseDate', 'serviceUpdate_serviceUpdateReleaseDate' - The date when the service update is initially available
--
-- 'serviceUpdateSeverity', 'serviceUpdate_serviceUpdateSeverity' - The severity of the service update
--
-- 'serviceUpdateStatus', 'serviceUpdate_serviceUpdateStatus' - The status of the service update
--
-- 'serviceUpdateType', 'serviceUpdate_serviceUpdateType' - Reflects the nature of the service update
newServiceUpdate ::
  ServiceUpdate
newServiceUpdate =
  ServiceUpdate'
    { autoUpdateAfterRecommendedApplyByDate =
        Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      estimatedUpdateTime = Prelude.Nothing,
      serviceUpdateDescription = Prelude.Nothing,
      serviceUpdateEndDate = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      serviceUpdateRecommendedApplyByDate =
        Prelude.Nothing,
      serviceUpdateReleaseDate = Prelude.Nothing,
      serviceUpdateSeverity = Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing,
      serviceUpdateType = Prelude.Nothing
    }

-- | Indicates whether the service update will be automatically applied once
-- the recommended apply-by date has expired.
serviceUpdate_autoUpdateAfterRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Bool)
serviceUpdate_autoUpdateAfterRecommendedApplyByDate = Lens.lens (\ServiceUpdate' {autoUpdateAfterRecommendedApplyByDate} -> autoUpdateAfterRecommendedApplyByDate) (\s@ServiceUpdate' {} a -> s {autoUpdateAfterRecommendedApplyByDate = a} :: ServiceUpdate)

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
serviceUpdate_engine :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_engine = Lens.lens (\ServiceUpdate' {engine} -> engine) (\s@ServiceUpdate' {} a -> s {engine = a} :: ServiceUpdate)

-- | The Elasticache engine version to which the update applies. Either Redis
-- or Memcached engine version
serviceUpdate_engineVersion :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_engineVersion = Lens.lens (\ServiceUpdate' {engineVersion} -> engineVersion) (\s@ServiceUpdate' {} a -> s {engineVersion = a} :: ServiceUpdate)

-- | The estimated length of time the service update will take
serviceUpdate_estimatedUpdateTime :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_estimatedUpdateTime = Lens.lens (\ServiceUpdate' {estimatedUpdateTime} -> estimatedUpdateTime) (\s@ServiceUpdate' {} a -> s {estimatedUpdateTime = a} :: ServiceUpdate)

-- | Provides details of the service update
serviceUpdate_serviceUpdateDescription :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_serviceUpdateDescription = Lens.lens (\ServiceUpdate' {serviceUpdateDescription} -> serviceUpdateDescription) (\s@ServiceUpdate' {} a -> s {serviceUpdateDescription = a} :: ServiceUpdate)

-- | The date after which the service update is no longer available
serviceUpdate_serviceUpdateEndDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_serviceUpdateEndDate = Lens.lens (\ServiceUpdate' {serviceUpdateEndDate} -> serviceUpdateEndDate) (\s@ServiceUpdate' {} a -> s {serviceUpdateEndDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Data._Time

-- | The unique ID of the service update
serviceUpdate_serviceUpdateName :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_serviceUpdateName = Lens.lens (\ServiceUpdate' {serviceUpdateName} -> serviceUpdateName) (\s@ServiceUpdate' {} a -> s {serviceUpdateName = a} :: ServiceUpdate)

-- | The recommendend date to apply the service update in order to ensure
-- compliance. For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
serviceUpdate_serviceUpdateRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_serviceUpdateRecommendedApplyByDate = Lens.lens (\ServiceUpdate' {serviceUpdateRecommendedApplyByDate} -> serviceUpdateRecommendedApplyByDate) (\s@ServiceUpdate' {} a -> s {serviceUpdateRecommendedApplyByDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Data._Time

-- | The date when the service update is initially available
serviceUpdate_serviceUpdateReleaseDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_serviceUpdateReleaseDate = Lens.lens (\ServiceUpdate' {serviceUpdateReleaseDate} -> serviceUpdateReleaseDate) (\s@ServiceUpdate' {} a -> s {serviceUpdateReleaseDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Data._Time

-- | The severity of the service update
serviceUpdate_serviceUpdateSeverity :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateSeverity)
serviceUpdate_serviceUpdateSeverity = Lens.lens (\ServiceUpdate' {serviceUpdateSeverity} -> serviceUpdateSeverity) (\s@ServiceUpdate' {} a -> s {serviceUpdateSeverity = a} :: ServiceUpdate)

-- | The status of the service update
serviceUpdate_serviceUpdateStatus :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateStatus)
serviceUpdate_serviceUpdateStatus = Lens.lens (\ServiceUpdate' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@ServiceUpdate' {} a -> s {serviceUpdateStatus = a} :: ServiceUpdate)

-- | Reflects the nature of the service update
serviceUpdate_serviceUpdateType :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateType)
serviceUpdate_serviceUpdateType = Lens.lens (\ServiceUpdate' {serviceUpdateType} -> serviceUpdateType) (\s@ServiceUpdate' {} a -> s {serviceUpdateType = a} :: ServiceUpdate)

instance Data.FromXML ServiceUpdate where
  parseXML x =
    ServiceUpdate'
      Prelude.<$> (x Data..@? "AutoUpdateAfterRecommendedApplyByDate")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "EstimatedUpdateTime")
      Prelude.<*> (x Data..@? "ServiceUpdateDescription")
      Prelude.<*> (x Data..@? "ServiceUpdateEndDate")
      Prelude.<*> (x Data..@? "ServiceUpdateName")
      Prelude.<*> (x Data..@? "ServiceUpdateRecommendedApplyByDate")
      Prelude.<*> (x Data..@? "ServiceUpdateReleaseDate")
      Prelude.<*> (x Data..@? "ServiceUpdateSeverity")
      Prelude.<*> (x Data..@? "ServiceUpdateStatus")
      Prelude.<*> (x Data..@? "ServiceUpdateType")

instance Prelude.Hashable ServiceUpdate where
  hashWithSalt _salt ServiceUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` autoUpdateAfterRecommendedApplyByDate
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` estimatedUpdateTime
      `Prelude.hashWithSalt` serviceUpdateDescription
      `Prelude.hashWithSalt` serviceUpdateEndDate
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` serviceUpdateRecommendedApplyByDate
      `Prelude.hashWithSalt` serviceUpdateReleaseDate
      `Prelude.hashWithSalt` serviceUpdateSeverity
      `Prelude.hashWithSalt` serviceUpdateStatus
      `Prelude.hashWithSalt` serviceUpdateType

instance Prelude.NFData ServiceUpdate where
  rnf ServiceUpdate' {..} =
    Prelude.rnf autoUpdateAfterRecommendedApplyByDate
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf estimatedUpdateTime
      `Prelude.seq` Prelude.rnf serviceUpdateDescription
      `Prelude.seq` Prelude.rnf serviceUpdateEndDate
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf serviceUpdateRecommendedApplyByDate
      `Prelude.seq` Prelude.rnf serviceUpdateReleaseDate
      `Prelude.seq` Prelude.rnf serviceUpdateSeverity
      `Prelude.seq` Prelude.rnf serviceUpdateStatus
      `Prelude.seq` Prelude.rnf serviceUpdateType
