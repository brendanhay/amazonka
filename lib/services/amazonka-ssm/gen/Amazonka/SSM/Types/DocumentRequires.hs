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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentRequires where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An SSM document required by the current document.
--
-- /See:/ 'newDocumentRequires' smart constructor.
data DocumentRequires = DocumentRequires'
  { -- | The document version required by the current document.
    version :: Prelude.Maybe Prelude.Text,
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
-- 'version', 'documentRequires_version' - The document version required by the current document.
--
-- 'name', 'documentRequires_name' - The name of the required SSM document. The name can be an Amazon
-- Resource Name (ARN).
newDocumentRequires ::
  -- | 'name'
  Prelude.Text ->
  DocumentRequires
newDocumentRequires pName_ =
  DocumentRequires'
    { version = Prelude.Nothing,
      name = pName_
    }

-- | The document version required by the current document.
documentRequires_version :: Lens.Lens' DocumentRequires (Prelude.Maybe Prelude.Text)
documentRequires_version = Lens.lens (\DocumentRequires' {version} -> version) (\s@DocumentRequires' {} a -> s {version = a} :: DocumentRequires)

-- | The name of the required SSM document. The name can be an Amazon
-- Resource Name (ARN).
documentRequires_name :: Lens.Lens' DocumentRequires Prelude.Text
documentRequires_name = Lens.lens (\DocumentRequires' {name} -> name) (\s@DocumentRequires' {} a -> s {name = a} :: DocumentRequires)

instance Core.FromJSON DocumentRequires where
  parseJSON =
    Core.withObject
      "DocumentRequires"
      ( \x ->
          DocumentRequires'
            Prelude.<$> (x Core..:? "Version")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable DocumentRequires where
  hashWithSalt _salt DocumentRequires' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData DocumentRequires where
  rnf DocumentRequires' {..} =
    Prelude.rnf version `Prelude.seq` Prelude.rnf name

instance Core.ToJSON DocumentRequires where
  toJSON DocumentRequires' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Version" Core..=) Prelude.<$> version,
            Prelude.Just ("Name" Core..= name)
          ]
      )
