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
-- Module      : Amazonka.SSM.Types.DocumentRequires
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentRequires where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An SSM document required by the current document.
--
-- /See:/ 'newDocumentRequires' smart constructor.
data DocumentRequires = DocumentRequires'
  { -- | The document type of the required SSM document.
    requireType :: Prelude.Maybe Prelude.Text,
    -- | The document version required by the current document.
    version :: Prelude.Maybe Prelude.Text,
    -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the required SSM document. The name can be an Amazon
    -- Resource Name (ARN).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentRequires' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requireType', 'documentRequires_requireType' - The document type of the required SSM document.
--
-- 'version', 'documentRequires_version' - The document version required by the current document.
--
-- 'versionName', 'documentRequires_versionName' - An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and can\'t be changed.
--
-- 'name', 'documentRequires_name' - The name of the required SSM document. The name can be an Amazon
-- Resource Name (ARN).
newDocumentRequires ::
  -- | 'name'
  Prelude.Text ->
  DocumentRequires
newDocumentRequires pName_ =
  DocumentRequires'
    { requireType = Prelude.Nothing,
      version = Prelude.Nothing,
      versionName = Prelude.Nothing,
      name = pName_
    }

-- | The document type of the required SSM document.
documentRequires_requireType :: Lens.Lens' DocumentRequires (Prelude.Maybe Prelude.Text)
documentRequires_requireType = Lens.lens (\DocumentRequires' {requireType} -> requireType) (\s@DocumentRequires' {} a -> s {requireType = a} :: DocumentRequires)

-- | The document version required by the current document.
documentRequires_version :: Lens.Lens' DocumentRequires (Prelude.Maybe Prelude.Text)
documentRequires_version = Lens.lens (\DocumentRequires' {version} -> version) (\s@DocumentRequires' {} a -> s {version = a} :: DocumentRequires)

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and can\'t be changed.
documentRequires_versionName :: Lens.Lens' DocumentRequires (Prelude.Maybe Prelude.Text)
documentRequires_versionName = Lens.lens (\DocumentRequires' {versionName} -> versionName) (\s@DocumentRequires' {} a -> s {versionName = a} :: DocumentRequires)

-- | The name of the required SSM document. The name can be an Amazon
-- Resource Name (ARN).
documentRequires_name :: Lens.Lens' DocumentRequires Prelude.Text
documentRequires_name = Lens.lens (\DocumentRequires' {name} -> name) (\s@DocumentRequires' {} a -> s {name = a} :: DocumentRequires)

instance Data.FromJSON DocumentRequires where
  parseJSON =
    Data.withObject
      "DocumentRequires"
      ( \x ->
          DocumentRequires'
            Prelude.<$> (x Data..:? "RequireType")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "VersionName")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable DocumentRequires where
  hashWithSalt _salt DocumentRequires' {..} =
    _salt
      `Prelude.hashWithSalt` requireType
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DocumentRequires where
  rnf DocumentRequires' {..} =
    Prelude.rnf requireType `Prelude.seq`
      Prelude.rnf version `Prelude.seq`
        Prelude.rnf versionName `Prelude.seq`
          Prelude.rnf name

instance Data.ToJSON DocumentRequires where
  toJSON DocumentRequires' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequireType" Data..=) Prelude.<$> requireType,
            ("Version" Data..=) Prelude.<$> version,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just ("Name" Data..= name)
          ]
      )
