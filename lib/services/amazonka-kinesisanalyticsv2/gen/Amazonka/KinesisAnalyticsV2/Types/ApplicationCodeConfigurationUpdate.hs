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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.CodeContentType
import Amazonka.KinesisAnalyticsV2.Types.CodeContentUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes code configuration updates for an application. This is
-- supported for a Flink-based Kinesis Data Analytics application or a
-- SQL-based Kinesis Data Analytics application.
--
-- /See:/ 'newApplicationCodeConfigurationUpdate' smart constructor.
data ApplicationCodeConfigurationUpdate = ApplicationCodeConfigurationUpdate'
  { -- | Describes updates to the code content type.
    codeContentTypeUpdate :: Prelude.Maybe CodeContentType,
    -- | Describes updates to the code content of an application.
    codeContentUpdate :: Prelude.Maybe CodeContentUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationCodeConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeContentTypeUpdate', 'applicationCodeConfigurationUpdate_codeContentTypeUpdate' - Describes updates to the code content type.
--
-- 'codeContentUpdate', 'applicationCodeConfigurationUpdate_codeContentUpdate' - Describes updates to the code content of an application.
newApplicationCodeConfigurationUpdate ::
  ApplicationCodeConfigurationUpdate
newApplicationCodeConfigurationUpdate =
  ApplicationCodeConfigurationUpdate'
    { codeContentTypeUpdate =
        Prelude.Nothing,
      codeContentUpdate = Prelude.Nothing
    }

-- | Describes updates to the code content type.
applicationCodeConfigurationUpdate_codeContentTypeUpdate :: Lens.Lens' ApplicationCodeConfigurationUpdate (Prelude.Maybe CodeContentType)
applicationCodeConfigurationUpdate_codeContentTypeUpdate = Lens.lens (\ApplicationCodeConfigurationUpdate' {codeContentTypeUpdate} -> codeContentTypeUpdate) (\s@ApplicationCodeConfigurationUpdate' {} a -> s {codeContentTypeUpdate = a} :: ApplicationCodeConfigurationUpdate)

-- | Describes updates to the code content of an application.
applicationCodeConfigurationUpdate_codeContentUpdate :: Lens.Lens' ApplicationCodeConfigurationUpdate (Prelude.Maybe CodeContentUpdate)
applicationCodeConfigurationUpdate_codeContentUpdate = Lens.lens (\ApplicationCodeConfigurationUpdate' {codeContentUpdate} -> codeContentUpdate) (\s@ApplicationCodeConfigurationUpdate' {} a -> s {codeContentUpdate = a} :: ApplicationCodeConfigurationUpdate)

instance
  Prelude.Hashable
    ApplicationCodeConfigurationUpdate
  where
  hashWithSalt
    _salt
    ApplicationCodeConfigurationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` codeContentTypeUpdate
        `Prelude.hashWithSalt` codeContentUpdate

instance
  Prelude.NFData
    ApplicationCodeConfigurationUpdate
  where
  rnf ApplicationCodeConfigurationUpdate' {..} =
    Prelude.rnf codeContentTypeUpdate
      `Prelude.seq` Prelude.rnf codeContentUpdate

instance
  Data.ToJSON
    ApplicationCodeConfigurationUpdate
  where
  toJSON ApplicationCodeConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CodeContentTypeUpdate" Data..=)
              Prelude.<$> codeContentTypeUpdate,
            ("CodeContentUpdate" Data..=)
              Prelude.<$> codeContentUpdate
          ]
      )
