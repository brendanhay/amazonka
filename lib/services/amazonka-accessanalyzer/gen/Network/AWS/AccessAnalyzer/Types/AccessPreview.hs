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
-- Module      : Network.AWS.AccessAnalyzer.Types.AccessPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.AccessPreview where

import Network.AWS.AccessAnalyzer.Types.AccessPreviewStatus
import Network.AWS.AccessAnalyzer.Types.AccessPreviewStatusReason
import Network.AWS.AccessAnalyzer.Types.Configuration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an access preview.
--
-- /See:/ 'newAccessPreview' smart constructor.
data AccessPreview = AccessPreview'
  { -- | Provides more details about the current status of the access preview.
    --
    -- For example, if the creation of the access preview fails, a @Failed@
    -- status is returned. This failure can be due to an internal issue with
    -- the analysis or due to an invalid resource configuration.
    statusReason :: Prelude.Maybe AccessPreviewStatusReason,
    -- | The ARN of the analyzer used to generate the access preview.
    analyzerArn :: Prelude.Text,
    -- | A map of resource ARNs for the proposed resource configuration.
    configurations :: Prelude.HashMap Prelude.Text Configuration,
    -- | The time at which the access preview was created.
    createdAt :: Core.POSIX,
    -- | The unique ID for the access preview.
    id :: Prelude.Text,
    -- | The status of the access preview.
    --
    -- -   @Creating@ - The access preview creation is in progress.
    --
    -- -   @Completed@ - The access preview is complete. You can preview
    --     findings for external access to the resource.
    --
    -- -   @Failed@ - The access preview creation has failed.
    status :: AccessPreviewStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'accessPreview_statusReason' - Provides more details about the current status of the access preview.
--
-- For example, if the creation of the access preview fails, a @Failed@
-- status is returned. This failure can be due to an internal issue with
-- the analysis or due to an invalid resource configuration.
--
-- 'analyzerArn', 'accessPreview_analyzerArn' - The ARN of the analyzer used to generate the access preview.
--
-- 'configurations', 'accessPreview_configurations' - A map of resource ARNs for the proposed resource configuration.
--
-- 'createdAt', 'accessPreview_createdAt' - The time at which the access preview was created.
--
-- 'id', 'accessPreview_id' - The unique ID for the access preview.
--
-- 'status', 'accessPreview_status' - The status of the access preview.
--
-- -   @Creating@ - The access preview creation is in progress.
--
-- -   @Completed@ - The access preview is complete. You can preview
--     findings for external access to the resource.
--
-- -   @Failed@ - The access preview creation has failed.
newAccessPreview ::
  -- | 'analyzerArn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  AccessPreviewStatus ->
  AccessPreview
newAccessPreview
  pAnalyzerArn_
  pCreatedAt_
  pId_
  pStatus_ =
    AccessPreview'
      { statusReason = Prelude.Nothing,
        analyzerArn = pAnalyzerArn_,
        configurations = Prelude.mempty,
        createdAt = Core._Time Lens.# pCreatedAt_,
        id = pId_,
        status = pStatus_
      }

-- | Provides more details about the current status of the access preview.
--
-- For example, if the creation of the access preview fails, a @Failed@
-- status is returned. This failure can be due to an internal issue with
-- the analysis or due to an invalid resource configuration.
accessPreview_statusReason :: Lens.Lens' AccessPreview (Prelude.Maybe AccessPreviewStatusReason)
accessPreview_statusReason = Lens.lens (\AccessPreview' {statusReason} -> statusReason) (\s@AccessPreview' {} a -> s {statusReason = a} :: AccessPreview)

-- | The ARN of the analyzer used to generate the access preview.
accessPreview_analyzerArn :: Lens.Lens' AccessPreview Prelude.Text
accessPreview_analyzerArn = Lens.lens (\AccessPreview' {analyzerArn} -> analyzerArn) (\s@AccessPreview' {} a -> s {analyzerArn = a} :: AccessPreview)

-- | A map of resource ARNs for the proposed resource configuration.
accessPreview_configurations :: Lens.Lens' AccessPreview (Prelude.HashMap Prelude.Text Configuration)
accessPreview_configurations = Lens.lens (\AccessPreview' {configurations} -> configurations) (\s@AccessPreview' {} a -> s {configurations = a} :: AccessPreview) Prelude.. Lens.coerced

-- | The time at which the access preview was created.
accessPreview_createdAt :: Lens.Lens' AccessPreview Prelude.UTCTime
accessPreview_createdAt = Lens.lens (\AccessPreview' {createdAt} -> createdAt) (\s@AccessPreview' {} a -> s {createdAt = a} :: AccessPreview) Prelude.. Core._Time

-- | The unique ID for the access preview.
accessPreview_id :: Lens.Lens' AccessPreview Prelude.Text
accessPreview_id = Lens.lens (\AccessPreview' {id} -> id) (\s@AccessPreview' {} a -> s {id = a} :: AccessPreview)

-- | The status of the access preview.
--
-- -   @Creating@ - The access preview creation is in progress.
--
-- -   @Completed@ - The access preview is complete. You can preview
--     findings for external access to the resource.
--
-- -   @Failed@ - The access preview creation has failed.
accessPreview_status :: Lens.Lens' AccessPreview AccessPreviewStatus
accessPreview_status = Lens.lens (\AccessPreview' {status} -> status) (\s@AccessPreview' {} a -> s {status = a} :: AccessPreview)

instance Core.FromJSON AccessPreview where
  parseJSON =
    Core.withObject
      "AccessPreview"
      ( \x ->
          AccessPreview'
            Prelude.<$> (x Core..:? "statusReason")
            Prelude.<*> (x Core..: "analyzerArn")
            Prelude.<*> (x Core..:? "configurations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable AccessPreview

instance Prelude.NFData AccessPreview
