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
-- Module      : Amazonka.Proton.Types.CompatibleEnvironmentTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.CompatibleEnvironmentTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Compatible environment template data.
--
-- /See:/ 'newCompatibleEnvironmentTemplate' smart constructor.
data CompatibleEnvironmentTemplate = CompatibleEnvironmentTemplate'
  { -- | The major version of the compatible environment template.
    majorVersion :: Prelude.Text,
    -- | The compatible environment template name.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompatibleEnvironmentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'compatibleEnvironmentTemplate_majorVersion' - The major version of the compatible environment template.
--
-- 'templateName', 'compatibleEnvironmentTemplate_templateName' - The compatible environment template name.
newCompatibleEnvironmentTemplate ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  CompatibleEnvironmentTemplate
newCompatibleEnvironmentTemplate
  pMajorVersion_
  pTemplateName_ =
    CompatibleEnvironmentTemplate'
      { majorVersion =
          pMajorVersion_,
        templateName = pTemplateName_
      }

-- | The major version of the compatible environment template.
compatibleEnvironmentTemplate_majorVersion :: Lens.Lens' CompatibleEnvironmentTemplate Prelude.Text
compatibleEnvironmentTemplate_majorVersion = Lens.lens (\CompatibleEnvironmentTemplate' {majorVersion} -> majorVersion) (\s@CompatibleEnvironmentTemplate' {} a -> s {majorVersion = a} :: CompatibleEnvironmentTemplate)

-- | The compatible environment template name.
compatibleEnvironmentTemplate_templateName :: Lens.Lens' CompatibleEnvironmentTemplate Prelude.Text
compatibleEnvironmentTemplate_templateName = Lens.lens (\CompatibleEnvironmentTemplate' {templateName} -> templateName) (\s@CompatibleEnvironmentTemplate' {} a -> s {templateName = a} :: CompatibleEnvironmentTemplate)

instance Data.FromJSON CompatibleEnvironmentTemplate where
  parseJSON =
    Data.withObject
      "CompatibleEnvironmentTemplate"
      ( \x ->
          CompatibleEnvironmentTemplate'
            Prelude.<$> (x Data..: "majorVersion")
            Prelude.<*> (x Data..: "templateName")
      )

instance
  Prelude.Hashable
    CompatibleEnvironmentTemplate
  where
  hashWithSalt _salt CompatibleEnvironmentTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData CompatibleEnvironmentTemplate where
  rnf CompatibleEnvironmentTemplate' {..} =
    Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf templateName
