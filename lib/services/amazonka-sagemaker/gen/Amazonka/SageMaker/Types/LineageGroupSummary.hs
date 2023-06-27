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
-- Module      : Amazonka.SageMaker.Types.LineageGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LineageGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lists a summary of the properties of a lineage group. A lineage group
-- provides a group of shareable lineage entity resources.
--
-- /See:/ 'newLineageGroupSummary' smart constructor.
data LineageGroupSummary = LineageGroupSummary'
  { -- | The creation time of the lineage group summary.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The display name of the lineage group summary.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The last modified time of the lineage group summary.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the lineage group resource.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the lineage group.
    lineageGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineageGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'lineageGroupSummary_creationTime' - The creation time of the lineage group summary.
--
-- 'displayName', 'lineageGroupSummary_displayName' - The display name of the lineage group summary.
--
-- 'lastModifiedTime', 'lineageGroupSummary_lastModifiedTime' - The last modified time of the lineage group summary.
--
-- 'lineageGroupArn', 'lineageGroupSummary_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group resource.
--
-- 'lineageGroupName', 'lineageGroupSummary_lineageGroupName' - The name or Amazon Resource Name (ARN) of the lineage group.
newLineageGroupSummary ::
  LineageGroupSummary
newLineageGroupSummary =
  LineageGroupSummary'
    { creationTime =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      lineageGroupName = Prelude.Nothing
    }

-- | The creation time of the lineage group summary.
lineageGroupSummary_creationTime :: Lens.Lens' LineageGroupSummary (Prelude.Maybe Prelude.UTCTime)
lineageGroupSummary_creationTime = Lens.lens (\LineageGroupSummary' {creationTime} -> creationTime) (\s@LineageGroupSummary' {} a -> s {creationTime = a} :: LineageGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The display name of the lineage group summary.
lineageGroupSummary_displayName :: Lens.Lens' LineageGroupSummary (Prelude.Maybe Prelude.Text)
lineageGroupSummary_displayName = Lens.lens (\LineageGroupSummary' {displayName} -> displayName) (\s@LineageGroupSummary' {} a -> s {displayName = a} :: LineageGroupSummary)

-- | The last modified time of the lineage group summary.
lineageGroupSummary_lastModifiedTime :: Lens.Lens' LineageGroupSummary (Prelude.Maybe Prelude.UTCTime)
lineageGroupSummary_lastModifiedTime = Lens.lens (\LineageGroupSummary' {lastModifiedTime} -> lastModifiedTime) (\s@LineageGroupSummary' {} a -> s {lastModifiedTime = a} :: LineageGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the lineage group resource.
lineageGroupSummary_lineageGroupArn :: Lens.Lens' LineageGroupSummary (Prelude.Maybe Prelude.Text)
lineageGroupSummary_lineageGroupArn = Lens.lens (\LineageGroupSummary' {lineageGroupArn} -> lineageGroupArn) (\s@LineageGroupSummary' {} a -> s {lineageGroupArn = a} :: LineageGroupSummary)

-- | The name or Amazon Resource Name (ARN) of the lineage group.
lineageGroupSummary_lineageGroupName :: Lens.Lens' LineageGroupSummary (Prelude.Maybe Prelude.Text)
lineageGroupSummary_lineageGroupName = Lens.lens (\LineageGroupSummary' {lineageGroupName} -> lineageGroupName) (\s@LineageGroupSummary' {} a -> s {lineageGroupName = a} :: LineageGroupSummary)

instance Data.FromJSON LineageGroupSummary where
  parseJSON =
    Data.withObject
      "LineageGroupSummary"
      ( \x ->
          LineageGroupSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LineageGroupArn")
            Prelude.<*> (x Data..:? "LineageGroupName")
      )

instance Prelude.Hashable LineageGroupSummary where
  hashWithSalt _salt LineageGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lineageGroupArn
      `Prelude.hashWithSalt` lineageGroupName

instance Prelude.NFData LineageGroupSummary where
  rnf LineageGroupSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf lineageGroupName
