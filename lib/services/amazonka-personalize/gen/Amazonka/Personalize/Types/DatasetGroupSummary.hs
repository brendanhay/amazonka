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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.Domain
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset group. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>
-- API.
--
-- /See:/ 'newDatasetGroupSummary' smart constructor.
data DatasetGroupSummary = DatasetGroupSummary'
  { -- | The date and time (in Unix time) that the dataset group was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The domain of a Domain dataset group.
    domain :: Prelude.Maybe Domain,
    -- | If creating a dataset group fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset group was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the dataset group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset group.
    --
    -- A dataset group can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING
    status :: Prelude.Maybe Prelude.Text
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
-- 'creationDateTime', 'datasetGroupSummary_creationDateTime' - The date and time (in Unix time) that the dataset group was created.
--
-- 'datasetGroupArn', 'datasetGroupSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
--
-- 'domain', 'datasetGroupSummary_domain' - The domain of a Domain dataset group.
--
-- 'failureReason', 'datasetGroupSummary_failureReason' - If creating a dataset group fails, the reason behind the failure.
--
-- 'lastUpdatedDateTime', 'datasetGroupSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset group was last
-- updated.
--
-- 'name', 'datasetGroupSummary_name' - The name of the dataset group.
--
-- 'status', 'datasetGroupSummary_status' - The status of the dataset group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
newDatasetGroupSummary ::
  DatasetGroupSummary
newDatasetGroupSummary =
  DatasetGroupSummary'
    { creationDateTime =
        Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      domain = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the dataset group was created.
datasetGroupSummary_creationDateTime :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.UTCTime)
datasetGroupSummary_creationDateTime = Lens.lens (\DatasetGroupSummary' {creationDateTime} -> creationDateTime) (\s@DatasetGroupSummary' {} a -> s {creationDateTime = a} :: DatasetGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset group.
datasetGroupSummary_datasetGroupArn :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_datasetGroupArn = Lens.lens (\DatasetGroupSummary' {datasetGroupArn} -> datasetGroupArn) (\s@DatasetGroupSummary' {} a -> s {datasetGroupArn = a} :: DatasetGroupSummary)

-- | The domain of a Domain dataset group.
datasetGroupSummary_domain :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Domain)
datasetGroupSummary_domain = Lens.lens (\DatasetGroupSummary' {domain} -> domain) (\s@DatasetGroupSummary' {} a -> s {domain = a} :: DatasetGroupSummary)

-- | If creating a dataset group fails, the reason behind the failure.
datasetGroupSummary_failureReason :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_failureReason = Lens.lens (\DatasetGroupSummary' {failureReason} -> failureReason) (\s@DatasetGroupSummary' {} a -> s {failureReason = a} :: DatasetGroupSummary)

-- | The date and time (in Unix time) that the dataset group was last
-- updated.
datasetGroupSummary_lastUpdatedDateTime :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.UTCTime)
datasetGroupSummary_lastUpdatedDateTime = Lens.lens (\DatasetGroupSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetGroupSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the dataset group.
datasetGroupSummary_name :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_name = Lens.lens (\DatasetGroupSummary' {name} -> name) (\s@DatasetGroupSummary' {} a -> s {name = a} :: DatasetGroupSummary)

-- | The status of the dataset group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
datasetGroupSummary_status :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_status = Lens.lens (\DatasetGroupSummary' {status} -> status) (\s@DatasetGroupSummary' {} a -> s {status = a} :: DatasetGroupSummary)

instance Data.FromJSON DatasetGroupSummary where
  parseJSON =
    Data.withObject
      "DatasetGroupSummary"
      ( \x ->
          DatasetGroupSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "datasetGroupArn")
            Prelude.<*> (x Data..:? "domain")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DatasetGroupSummary where
  hashWithSalt _salt DatasetGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetGroupSummary where
  rnf DatasetGroupSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
