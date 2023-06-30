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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information about the revision of the SNOMED-CT ontology in the
-- response. Specifically, the details include the SNOMED-CT edition,
-- language, and version date.
--
-- /See:/ 'newSNOMEDCTDetails' smart constructor.
data SNOMEDCTDetails = SNOMEDCTDetails'
  { -- | The edition of SNOMED-CT used. The edition used for the InferSNOMEDCT
    -- editions is the US edition.
    edition :: Prelude.Maybe Prelude.Text,
    -- | The language used in the SNOMED-CT ontology. All Amazon Comprehend
    -- Medical operations are US English (en).
    language :: Prelude.Maybe Prelude.Text,
    -- | The version date of the SNOMED-CT ontology used.
    versionDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNOMEDCTDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edition', 'sNOMEDCTDetails_edition' - The edition of SNOMED-CT used. The edition used for the InferSNOMEDCT
-- editions is the US edition.
--
-- 'language', 'sNOMEDCTDetails_language' - The language used in the SNOMED-CT ontology. All Amazon Comprehend
-- Medical operations are US English (en).
--
-- 'versionDate', 'sNOMEDCTDetails_versionDate' - The version date of the SNOMED-CT ontology used.
newSNOMEDCTDetails ::
  SNOMEDCTDetails
newSNOMEDCTDetails =
  SNOMEDCTDetails'
    { edition = Prelude.Nothing,
      language = Prelude.Nothing,
      versionDate = Prelude.Nothing
    }

-- | The edition of SNOMED-CT used. The edition used for the InferSNOMEDCT
-- editions is the US edition.
sNOMEDCTDetails_edition :: Lens.Lens' SNOMEDCTDetails (Prelude.Maybe Prelude.Text)
sNOMEDCTDetails_edition = Lens.lens (\SNOMEDCTDetails' {edition} -> edition) (\s@SNOMEDCTDetails' {} a -> s {edition = a} :: SNOMEDCTDetails)

-- | The language used in the SNOMED-CT ontology. All Amazon Comprehend
-- Medical operations are US English (en).
sNOMEDCTDetails_language :: Lens.Lens' SNOMEDCTDetails (Prelude.Maybe Prelude.Text)
sNOMEDCTDetails_language = Lens.lens (\SNOMEDCTDetails' {language} -> language) (\s@SNOMEDCTDetails' {} a -> s {language = a} :: SNOMEDCTDetails)

-- | The version date of the SNOMED-CT ontology used.
sNOMEDCTDetails_versionDate :: Lens.Lens' SNOMEDCTDetails (Prelude.Maybe Prelude.Text)
sNOMEDCTDetails_versionDate = Lens.lens (\SNOMEDCTDetails' {versionDate} -> versionDate) (\s@SNOMEDCTDetails' {} a -> s {versionDate = a} :: SNOMEDCTDetails)

instance Data.FromJSON SNOMEDCTDetails where
  parseJSON =
    Data.withObject
      "SNOMEDCTDetails"
      ( \x ->
          SNOMEDCTDetails'
            Prelude.<$> (x Data..:? "Edition")
            Prelude.<*> (x Data..:? "Language")
            Prelude.<*> (x Data..:? "VersionDate")
      )

instance Prelude.Hashable SNOMEDCTDetails where
  hashWithSalt _salt SNOMEDCTDetails' {..} =
    _salt
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` versionDate

instance Prelude.NFData SNOMEDCTDetails where
  rnf SNOMEDCTDetails' {..} =
    Prelude.rnf edition
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf versionDate
