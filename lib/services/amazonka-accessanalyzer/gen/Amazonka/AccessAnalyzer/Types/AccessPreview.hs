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
-- Module      : Amazonka.AccessAnalyzer.Types.AccessPreview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AccessPreview where

import Amazonka.AccessAnalyzer.Types.AccessPreviewStatus
import Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReason
import Amazonka.AccessAnalyzer.Types.Configuration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    -- | The unique ID for the access preview.
    id :: Prelude.Text,
    -- | The ARN of the analyzer used to generate the access preview.
    analyzerArn :: Prelude.Text,
    -- | A map of resource ARNs for the proposed resource configuration.
    configurations :: Prelude.HashMap Prelude.Text Configuration,
    -- | The time at which the access preview was created.
    createdAt :: Data.ISO8601,
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
-- 'id', 'accessPreview_id' - The unique ID for the access preview.
--
-- 'analyzerArn', 'accessPreview_analyzerArn' - The ARN of the analyzer used to generate the access preview.
--
-- 'configurations', 'accessPreview_configurations' - A map of resource ARNs for the proposed resource configuration.
--
-- 'createdAt', 'accessPreview_createdAt' - The time at which the access preview was created.
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
  -- | 'id'
  Prelude.Text ->
  -- | 'analyzerArn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'status'
  AccessPreviewStatus ->
  AccessPreview
newAccessPreview
  pId_
  pAnalyzerArn_
  pCreatedAt_
  pStatus_ =
    AccessPreview'
      { statusReason = Prelude.Nothing,
        id = pId_,
        analyzerArn = pAnalyzerArn_,
        configurations = Prelude.mempty,
        createdAt = Data._Time Lens.# pCreatedAt_,
        status = pStatus_
      }

-- | Provides more details about the current status of the access preview.
--
-- For example, if the creation of the access preview fails, a @Failed@
-- status is returned. This failure can be due to an internal issue with
-- the analysis or due to an invalid resource configuration.
accessPreview_statusReason :: Lens.Lens' AccessPreview (Prelude.Maybe AccessPreviewStatusReason)
accessPreview_statusReason = Lens.lens (\AccessPreview' {statusReason} -> statusReason) (\s@AccessPreview' {} a -> s {statusReason = a} :: AccessPreview)

-- | The unique ID for the access preview.
accessPreview_id :: Lens.Lens' AccessPreview Prelude.Text
accessPreview_id = Lens.lens (\AccessPreview' {id} -> id) (\s@AccessPreview' {} a -> s {id = a} :: AccessPreview)

-- | The ARN of the analyzer used to generate the access preview.
accessPreview_analyzerArn :: Lens.Lens' AccessPreview Prelude.Text
accessPreview_analyzerArn = Lens.lens (\AccessPreview' {analyzerArn} -> analyzerArn) (\s@AccessPreview' {} a -> s {analyzerArn = a} :: AccessPreview)

-- | A map of resource ARNs for the proposed resource configuration.
accessPreview_configurations :: Lens.Lens' AccessPreview (Prelude.HashMap Prelude.Text Configuration)
accessPreview_configurations = Lens.lens (\AccessPreview' {configurations} -> configurations) (\s@AccessPreview' {} a -> s {configurations = a} :: AccessPreview) Prelude.. Lens.coerced

-- | The time at which the access preview was created.
accessPreview_createdAt :: Lens.Lens' AccessPreview Prelude.UTCTime
accessPreview_createdAt = Lens.lens (\AccessPreview' {createdAt} -> createdAt) (\s@AccessPreview' {} a -> s {createdAt = a} :: AccessPreview) Prelude.. Data._Time

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

instance Data.FromJSON AccessPreview where
  parseJSON =
    Data.withObject
      "AccessPreview"
      ( \x ->
          AccessPreview'
            Prelude.<$> (x Data..:? "statusReason")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "analyzerArn")
            Prelude.<*> (x Data..:? "configurations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable AccessPreview where
  hashWithSalt _salt AccessPreview' {..} =
    _salt
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` analyzerArn
      `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData AccessPreview where
  rnf AccessPreview' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf analyzerArn
      `Prelude.seq` Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf status
