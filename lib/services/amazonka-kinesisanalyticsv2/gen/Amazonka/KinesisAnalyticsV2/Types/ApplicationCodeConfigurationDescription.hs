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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.CodeContentDescription
import Amazonka.KinesisAnalyticsV2.Types.CodeContentType
import qualified Amazonka.Prelude as Prelude

-- | Describes code configuration for an application.
--
-- /See:/ 'newApplicationCodeConfigurationDescription' smart constructor.
data ApplicationCodeConfigurationDescription = ApplicationCodeConfigurationDescription'
  { -- | Describes details about the location and format of the application code.
    codeContentDescription :: Prelude.Maybe CodeContentDescription,
    -- | Specifies whether the code content is in text or zip format.
    codeContentType :: CodeContentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationCodeConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeContentDescription', 'applicationCodeConfigurationDescription_codeContentDescription' - Describes details about the location and format of the application code.
--
-- 'codeContentType', 'applicationCodeConfigurationDescription_codeContentType' - Specifies whether the code content is in text or zip format.
newApplicationCodeConfigurationDescription ::
  -- | 'codeContentType'
  CodeContentType ->
  ApplicationCodeConfigurationDescription
newApplicationCodeConfigurationDescription
  pCodeContentType_ =
    ApplicationCodeConfigurationDescription'
      { codeContentDescription =
          Prelude.Nothing,
        codeContentType =
          pCodeContentType_
      }

-- | Describes details about the location and format of the application code.
applicationCodeConfigurationDescription_codeContentDescription :: Lens.Lens' ApplicationCodeConfigurationDescription (Prelude.Maybe CodeContentDescription)
applicationCodeConfigurationDescription_codeContentDescription = Lens.lens (\ApplicationCodeConfigurationDescription' {codeContentDescription} -> codeContentDescription) (\s@ApplicationCodeConfigurationDescription' {} a -> s {codeContentDescription = a} :: ApplicationCodeConfigurationDescription)

-- | Specifies whether the code content is in text or zip format.
applicationCodeConfigurationDescription_codeContentType :: Lens.Lens' ApplicationCodeConfigurationDescription CodeContentType
applicationCodeConfigurationDescription_codeContentType = Lens.lens (\ApplicationCodeConfigurationDescription' {codeContentType} -> codeContentType) (\s@ApplicationCodeConfigurationDescription' {} a -> s {codeContentType = a} :: ApplicationCodeConfigurationDescription)

instance
  Core.FromJSON
    ApplicationCodeConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "ApplicationCodeConfigurationDescription"
      ( \x ->
          ApplicationCodeConfigurationDescription'
            Prelude.<$> (x Core..:? "CodeContentDescription")
            Prelude.<*> (x Core..: "CodeContentType")
      )

instance
  Prelude.Hashable
    ApplicationCodeConfigurationDescription
  where
  hashWithSalt
    _salt
    ApplicationCodeConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` codeContentDescription
        `Prelude.hashWithSalt` codeContentType

instance
  Prelude.NFData
    ApplicationCodeConfigurationDescription
  where
  rnf ApplicationCodeConfigurationDescription' {..} =
    Prelude.rnf codeContentDescription
      `Prelude.seq` Prelude.rnf codeContentType
