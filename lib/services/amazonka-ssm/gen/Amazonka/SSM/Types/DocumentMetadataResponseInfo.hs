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
-- Module      : Amazonka.SSM.Types.DocumentMetadataResponseInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentMetadataResponseInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentReviewerResponseSource

-- | Details about the response to a document review request.
--
-- /See:/ 'newDocumentMetadataResponseInfo' smart constructor.
data DocumentMetadataResponseInfo = DocumentMetadataResponseInfo'
  { -- | Details about a reviewer\'s response to a document review request.
    reviewerResponse :: Prelude.Maybe [DocumentReviewerResponseSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentMetadataResponseInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reviewerResponse', 'documentMetadataResponseInfo_reviewerResponse' - Details about a reviewer\'s response to a document review request.
newDocumentMetadataResponseInfo ::
  DocumentMetadataResponseInfo
newDocumentMetadataResponseInfo =
  DocumentMetadataResponseInfo'
    { reviewerResponse =
        Prelude.Nothing
    }

-- | Details about a reviewer\'s response to a document review request.
documentMetadataResponseInfo_reviewerResponse :: Lens.Lens' DocumentMetadataResponseInfo (Prelude.Maybe [DocumentReviewerResponseSource])
documentMetadataResponseInfo_reviewerResponse = Lens.lens (\DocumentMetadataResponseInfo' {reviewerResponse} -> reviewerResponse) (\s@DocumentMetadataResponseInfo' {} a -> s {reviewerResponse = a} :: DocumentMetadataResponseInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DocumentMetadataResponseInfo where
  parseJSON =
    Data.withObject
      "DocumentMetadataResponseInfo"
      ( \x ->
          DocumentMetadataResponseInfo'
            Prelude.<$> ( x Data..:? "ReviewerResponse"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DocumentMetadataResponseInfo
  where
  hashWithSalt _salt DocumentMetadataResponseInfo' {..} =
    _salt `Prelude.hashWithSalt` reviewerResponse

instance Prelude.NFData DocumentMetadataResponseInfo where
  rnf DocumentMetadataResponseInfo' {..} =
    Prelude.rnf reviewerResponse
