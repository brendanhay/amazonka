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
-- Module      : Amazonka.Proton.Types.EnvironmentTemplateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentTemplateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A search filter for environment templates.
--
-- /See:/ 'newEnvironmentTemplateFilter' smart constructor.
data EnvironmentTemplateFilter = EnvironmentTemplateFilter'
  { -- | Include @majorVersion@ to filter search for a major version.
    majorVersion :: Prelude.Text,
    -- | Include @templateName@ to filter search for a template name.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentTemplateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'environmentTemplateFilter_majorVersion' - Include @majorVersion@ to filter search for a major version.
--
-- 'templateName', 'environmentTemplateFilter_templateName' - Include @templateName@ to filter search for a template name.
newEnvironmentTemplateFilter ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  EnvironmentTemplateFilter
newEnvironmentTemplateFilter
  pMajorVersion_
  pTemplateName_ =
    EnvironmentTemplateFilter'
      { majorVersion =
          pMajorVersion_,
        templateName = pTemplateName_
      }

-- | Include @majorVersion@ to filter search for a major version.
environmentTemplateFilter_majorVersion :: Lens.Lens' EnvironmentTemplateFilter Prelude.Text
environmentTemplateFilter_majorVersion = Lens.lens (\EnvironmentTemplateFilter' {majorVersion} -> majorVersion) (\s@EnvironmentTemplateFilter' {} a -> s {majorVersion = a} :: EnvironmentTemplateFilter)

-- | Include @templateName@ to filter search for a template name.
environmentTemplateFilter_templateName :: Lens.Lens' EnvironmentTemplateFilter Prelude.Text
environmentTemplateFilter_templateName = Lens.lens (\EnvironmentTemplateFilter' {templateName} -> templateName) (\s@EnvironmentTemplateFilter' {} a -> s {templateName = a} :: EnvironmentTemplateFilter)

instance Prelude.Hashable EnvironmentTemplateFilter where
  hashWithSalt _salt EnvironmentTemplateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData EnvironmentTemplateFilter where
  rnf EnvironmentTemplateFilter' {..} =
    Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToJSON EnvironmentTemplateFilter where
  toJSON EnvironmentTemplateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("majorVersion" Data..= majorVersion),
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )
