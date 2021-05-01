{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.Types.Segment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Segment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A segment from a trace that has been ingested by the X-Ray service. The
-- segment can be compiled from documents uploaded with PutTraceSegments,
-- or an @inferred@ segment for a downstream service, generated from a
-- subsegment sent by the service that called it.
--
-- For the full segment document schema, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents>
-- in the /AWS X-Ray Developer Guide/.
--
-- /See:/ 'newSegment' smart constructor.
data Segment = Segment'
  { -- | The segment\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The segment document.
    document :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Segment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'segment_id' - The segment\'s ID.
--
-- 'document', 'segment_document' - The segment document.
newSegment ::
  Segment
newSegment =
  Segment'
    { id = Prelude.Nothing,
      document = Prelude.Nothing
    }

-- | The segment\'s ID.
segment_id :: Lens.Lens' Segment (Prelude.Maybe Prelude.Text)
segment_id = Lens.lens (\Segment' {id} -> id) (\s@Segment' {} a -> s {id = a} :: Segment)

-- | The segment document.
segment_document :: Lens.Lens' Segment (Prelude.Maybe Prelude.Text)
segment_document = Lens.lens (\Segment' {document} -> document) (\s@Segment' {} a -> s {document = a} :: Segment)

instance Prelude.FromJSON Segment where
  parseJSON =
    Prelude.withObject
      "Segment"
      ( \x ->
          Segment'
            Prelude.<$> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Document")
      )

instance Prelude.Hashable Segment

instance Prelude.NFData Segment
