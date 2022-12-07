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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.CodeContent
import Amazonka.KinesisAnalyticsV2.Types.CodeContentType
import qualified Amazonka.Prelude as Prelude

-- | Describes code configuration for an application.
--
-- /See:/ 'newApplicationCodeConfiguration' smart constructor.
data ApplicationCodeConfiguration = ApplicationCodeConfiguration'
  { -- | The location and type of the application code.
    codeContent :: Prelude.Maybe CodeContent,
    -- | Specifies whether the code content is in text or zip format.
    codeContentType :: CodeContentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationCodeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeContent', 'applicationCodeConfiguration_codeContent' - The location and type of the application code.
--
-- 'codeContentType', 'applicationCodeConfiguration_codeContentType' - Specifies whether the code content is in text or zip format.
newApplicationCodeConfiguration ::
  -- | 'codeContentType'
  CodeContentType ->
  ApplicationCodeConfiguration
newApplicationCodeConfiguration pCodeContentType_ =
  ApplicationCodeConfiguration'
    { codeContent =
        Prelude.Nothing,
      codeContentType = pCodeContentType_
    }

-- | The location and type of the application code.
applicationCodeConfiguration_codeContent :: Lens.Lens' ApplicationCodeConfiguration (Prelude.Maybe CodeContent)
applicationCodeConfiguration_codeContent = Lens.lens (\ApplicationCodeConfiguration' {codeContent} -> codeContent) (\s@ApplicationCodeConfiguration' {} a -> s {codeContent = a} :: ApplicationCodeConfiguration)

-- | Specifies whether the code content is in text or zip format.
applicationCodeConfiguration_codeContentType :: Lens.Lens' ApplicationCodeConfiguration CodeContentType
applicationCodeConfiguration_codeContentType = Lens.lens (\ApplicationCodeConfiguration' {codeContentType} -> codeContentType) (\s@ApplicationCodeConfiguration' {} a -> s {codeContentType = a} :: ApplicationCodeConfiguration)

instance
  Prelude.Hashable
    ApplicationCodeConfiguration
  where
  hashWithSalt _salt ApplicationCodeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` codeContent
      `Prelude.hashWithSalt` codeContentType

instance Prelude.NFData ApplicationCodeConfiguration where
  rnf ApplicationCodeConfiguration' {..} =
    Prelude.rnf codeContent
      `Prelude.seq` Prelude.rnf codeContentType

instance Data.ToJSON ApplicationCodeConfiguration where
  toJSON ApplicationCodeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CodeContent" Data..=) Prelude.<$> codeContent,
            Prelude.Just
              ("CodeContentType" Data..= codeContentType)
          ]
      )
