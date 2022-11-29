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
-- Module      : Amazonka.Personalize.Types.DatasetGroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.Domain
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset group. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>
-- API.
--
-- /See:/ 'newDatasetGroupSummary' smart constructor.
data DatasetGroupSummary = DatasetGroupSummary'
  { -- | The name of the dataset group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset group was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The domain of a Domain dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The status of the dataset group.
    --
    -- A dataset group can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset group was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | If creating a dataset group fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'datasetGroupSummary_name' - The name of the dataset group.
--
-- 'creationDateTime', 'datasetGroupSummary_creationDateTime' - The date and time (in Unix time) that the dataset group was created.
--
-- 'domain', 'datasetGroupSummary_domain' - The domain of a Domain dataset group.
--
-- 'status', 'datasetGroupSummary_status' - The status of the dataset group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
--
-- 'datasetGroupArn', 'datasetGroupSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
--
-- 'lastUpdatedDateTime', 'datasetGroupSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset group was last
-- updated.
--
-- 'failureReason', 'datasetGroupSummary_failureReason' - If creating a dataset group fails, the reason behind the failure.
newDatasetGroupSummary ::
  DatasetGroupSummary
newDatasetGroupSummary =
  DatasetGroupSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      domain = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The name of the dataset group.
datasetGroupSummary_name :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_name = Lens.lens (\DatasetGroupSummary' {name} -> name) (\s@DatasetGroupSummary' {} a -> s {name = a} :: DatasetGroupSummary)

-- | The date and time (in Unix time) that the dataset group was created.
datasetGroupSummary_creationDateTime :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.UTCTime)
datasetGroupSummary_creationDateTime = Lens.lens (\DatasetGroupSummary' {creationDateTime} -> creationDateTime) (\s@DatasetGroupSummary' {} a -> s {creationDateTime = a} :: DatasetGroupSummary) Prelude.. Lens.mapping Core._Time

-- | The domain of a Domain dataset group.
datasetGroupSummary_domain :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Domain)
datasetGroupSummary_domain = Lens.lens (\DatasetGroupSummary' {domain} -> domain) (\s@DatasetGroupSummary' {} a -> s {domain = a} :: DatasetGroupSummary)

-- | The status of the dataset group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
datasetGroupSummary_status :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_status = Lens.lens (\DatasetGroupSummary' {status} -> status) (\s@DatasetGroupSummary' {} a -> s {status = a} :: DatasetGroupSummary)

-- | The Amazon Resource Name (ARN) of the dataset group.
datasetGroupSummary_datasetGroupArn :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_datasetGroupArn = Lens.lens (\DatasetGroupSummary' {datasetGroupArn} -> datasetGroupArn) (\s@DatasetGroupSummary' {} a -> s {datasetGroupArn = a} :: DatasetGroupSummary)

-- | The date and time (in Unix time) that the dataset group was last
-- updated.
datasetGroupSummary_lastUpdatedDateTime :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.UTCTime)
datasetGroupSummary_lastUpdatedDateTime = Lens.lens (\DatasetGroupSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetGroupSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetGroupSummary) Prelude.. Lens.mapping Core._Time

-- | If creating a dataset group fails, the reason behind the failure.
datasetGroupSummary_failureReason :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_failureReason = Lens.lens (\DatasetGroupSummary' {failureReason} -> failureReason) (\s@DatasetGroupSummary' {} a -> s {failureReason = a} :: DatasetGroupSummary)

instance Core.FromJSON DatasetGroupSummary where
  parseJSON =
    Core.withObject
      "DatasetGroupSummary"
      ( \x ->
          DatasetGroupSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "domain")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "datasetGroupArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable DatasetGroupSummary where
  hashWithSalt _salt DatasetGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DatasetGroupSummary where
  rnf DatasetGroupSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
