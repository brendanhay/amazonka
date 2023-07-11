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
-- Module      : Amazonka.S3.Types.CompletedMultipartUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CompletedMultipartUpload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.CompletedPart

-- | The container for the completed multipart upload details.
--
-- /See:/ 'newCompletedMultipartUpload' smart constructor.
data CompletedMultipartUpload = CompletedMultipartUpload'
  { -- | Array of CompletedPart data types.
    --
    -- If you do not supply a valid @Part@ with your request, the service sends
    -- back an HTTP 400 response.
    parts :: Prelude.Maybe (Prelude.NonEmpty CompletedPart)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompletedMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parts', 'completedMultipartUpload_parts' - Array of CompletedPart data types.
--
-- If you do not supply a valid @Part@ with your request, the service sends
-- back an HTTP 400 response.
newCompletedMultipartUpload ::
  CompletedMultipartUpload
newCompletedMultipartUpload =
  CompletedMultipartUpload' {parts = Prelude.Nothing}

-- | Array of CompletedPart data types.
--
-- If you do not supply a valid @Part@ with your request, the service sends
-- back an HTTP 400 response.
completedMultipartUpload_parts :: Lens.Lens' CompletedMultipartUpload (Prelude.Maybe (Prelude.NonEmpty CompletedPart))
completedMultipartUpload_parts = Lens.lens (\CompletedMultipartUpload' {parts} -> parts) (\s@CompletedMultipartUpload' {} a -> s {parts = a} :: CompletedMultipartUpload) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable CompletedMultipartUpload where
  hashWithSalt _salt CompletedMultipartUpload' {..} =
    _salt `Prelude.hashWithSalt` parts

instance Prelude.NFData CompletedMultipartUpload where
  rnf CompletedMultipartUpload' {..} = Prelude.rnf parts

instance Data.ToXML CompletedMultipartUpload where
  toXML CompletedMultipartUpload' {..} =
    Prelude.mconcat
      [ Data.toXML
          (Data.toXMLList "Part" Prelude.<$> parts)
      ]
