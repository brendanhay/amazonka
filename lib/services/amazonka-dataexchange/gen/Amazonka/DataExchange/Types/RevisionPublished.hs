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
-- Module      : Amazonka.DataExchange.Types.RevisionPublished
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.RevisionPublished where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the published revision.
--
-- /See:/ 'newRevisionPublished' smart constructor.
data RevisionPublished = RevisionPublished'
  { -- | The data set ID of the published revision.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevisionPublished' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'revisionPublished_dataSetId' - The data set ID of the published revision.
newRevisionPublished ::
  -- | 'dataSetId'
  Prelude.Text ->
  RevisionPublished
newRevisionPublished pDataSetId_ =
  RevisionPublished' {dataSetId = pDataSetId_}

-- | The data set ID of the published revision.
revisionPublished_dataSetId :: Lens.Lens' RevisionPublished Prelude.Text
revisionPublished_dataSetId = Lens.lens (\RevisionPublished' {dataSetId} -> dataSetId) (\s@RevisionPublished' {} a -> s {dataSetId = a} :: RevisionPublished)

instance Data.FromJSON RevisionPublished where
  parseJSON =
    Data.withObject
      "RevisionPublished"
      ( \x ->
          RevisionPublished'
            Prelude.<$> (x Data..: "DataSetId")
      )

instance Prelude.Hashable RevisionPublished where
  hashWithSalt _salt RevisionPublished' {..} =
    _salt `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData RevisionPublished where
  rnf RevisionPublished' {..} = Prelude.rnf dataSetId

instance Data.ToJSON RevisionPublished where
  toJSON RevisionPublished' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DataSetId" Data..= dataSetId)]
      )
