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
-- Module      : Amazonka.XRay.Types.Segment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.Segment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A segment from a trace that has been ingested by the X-Ray service. The
-- segment can be compiled from documents uploaded with
-- <https://docs.aws.amazon.com/xray/latest/api/API_PutTraceSegments.html PutTraceSegments>,
-- or an @inferred@ segment for a downstream service, generated from a
-- subsegment sent by the service that called it.
--
-- For the full segment document schema, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html Amazon Web Services X-Ray Segment Documents>
-- in the /Amazon Web Services X-Ray Developer Guide/.
--
-- /See:/ 'newSegment' smart constructor.
data Segment = Segment'
  { -- | The segment document.
    document :: Prelude.Maybe Prelude.Text,
    -- | The segment\'s ID.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Segment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'segment_document' - The segment document.
--
-- 'id', 'segment_id' - The segment\'s ID.
newSegment ::
  Segment
newSegment =
  Segment'
    { document = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The segment document.
segment_document :: Lens.Lens' Segment (Prelude.Maybe Prelude.Text)
segment_document = Lens.lens (\Segment' {document} -> document) (\s@Segment' {} a -> s {document = a} :: Segment)

-- | The segment\'s ID.
segment_id :: Lens.Lens' Segment (Prelude.Maybe Prelude.Text)
segment_id = Lens.lens (\Segment' {id} -> id) (\s@Segment' {} a -> s {id = a} :: Segment)

instance Data.FromJSON Segment where
  parseJSON =
    Data.withObject
      "Segment"
      ( \x ->
          Segment'
            Prelude.<$> (x Data..:? "Document")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable Segment where
  hashWithSalt _salt Segment' {..} =
    _salt
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` id

instance Prelude.NFData Segment where
  rnf Segment' {..} =
    Prelude.rnf document `Prelude.seq` Prelude.rnf id
