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
-- Module      : Amazonka.Textract.Types.IdentityDocumentField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.IdentityDocumentField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.AnalyzeIDDetections

-- | Structure containing both the normalized type of the extracted
-- information and the text associated with it. These are extracted as Type
-- and Value respectively.
--
-- /See:/ 'newIdentityDocumentField' smart constructor.
data IdentityDocumentField = IdentityDocumentField'
  { type' :: Prelude.Maybe AnalyzeIDDetections,
    valueDetection :: Prelude.Maybe AnalyzeIDDetections
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityDocumentField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'identityDocumentField_type' - Undocumented member.
--
-- 'valueDetection', 'identityDocumentField_valueDetection' - Undocumented member.
newIdentityDocumentField ::
  IdentityDocumentField
newIdentityDocumentField =
  IdentityDocumentField'
    { type' = Prelude.Nothing,
      valueDetection = Prelude.Nothing
    }

-- | Undocumented member.
identityDocumentField_type :: Lens.Lens' IdentityDocumentField (Prelude.Maybe AnalyzeIDDetections)
identityDocumentField_type = Lens.lens (\IdentityDocumentField' {type'} -> type') (\s@IdentityDocumentField' {} a -> s {type' = a} :: IdentityDocumentField)

-- | Undocumented member.
identityDocumentField_valueDetection :: Lens.Lens' IdentityDocumentField (Prelude.Maybe AnalyzeIDDetections)
identityDocumentField_valueDetection = Lens.lens (\IdentityDocumentField' {valueDetection} -> valueDetection) (\s@IdentityDocumentField' {} a -> s {valueDetection = a} :: IdentityDocumentField)

instance Data.FromJSON IdentityDocumentField where
  parseJSON =
    Data.withObject
      "IdentityDocumentField"
      ( \x ->
          IdentityDocumentField'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "ValueDetection")
      )

instance Prelude.Hashable IdentityDocumentField where
  hashWithSalt _salt IdentityDocumentField' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` valueDetection

instance Prelude.NFData IdentityDocumentField where
  rnf IdentityDocumentField' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf valueDetection
