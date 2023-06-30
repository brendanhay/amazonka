{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.ExportLens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Export an existing lens.
--
-- Lenses are defined in JSON. For more information, see
-- <https://docs.aws.amazon.com/wellarchitected/latest/userguide/lenses-format-specification.html JSON format specification>
-- in the /Well-Architected Tool User Guide/. Only the owner of a lens can
-- export it.
--
-- __Disclaimer__
--
-- Do not include or gather personal identifiable information (PII) of end
-- users or other identifiable individuals in or via your custom lenses. If
-- your custom lens or those shared with you and used in your account do
-- include or collect PII you are responsible for: ensuring that the
-- included PII is processed in accordance with applicable law, providing
-- adequate privacy notices, and obtaining necessary consents for
-- processing such data.
module Amazonka.WellArchitected.ExportLens
  ( -- * Creating a Request
    ExportLens (..),
    newExportLens,

    -- * Request Lenses
    exportLens_lensVersion,
    exportLens_lensAlias,

    -- * Destructuring the Response
    ExportLensResponse (..),
    newExportLensResponse,

    -- * Response Lenses
    exportLensResponse_lensJSON,
    exportLensResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newExportLens' smart constructor.
data ExportLens = ExportLens'
  { -- | The lens version to be exported.
    lensVersion :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportLens' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensVersion', 'exportLens_lensVersion' - The lens version to be exported.
--
-- 'lensAlias', 'exportLens_lensAlias' - Undocumented member.
newExportLens ::
  -- | 'lensAlias'
  Prelude.Text ->
  ExportLens
newExportLens pLensAlias_ =
  ExportLens'
    { lensVersion = Prelude.Nothing,
      lensAlias = pLensAlias_
    }

-- | The lens version to be exported.
exportLens_lensVersion :: Lens.Lens' ExportLens (Prelude.Maybe Prelude.Text)
exportLens_lensVersion = Lens.lens (\ExportLens' {lensVersion} -> lensVersion) (\s@ExportLens' {} a -> s {lensVersion = a} :: ExportLens)

-- | Undocumented member.
exportLens_lensAlias :: Lens.Lens' ExportLens Prelude.Text
exportLens_lensAlias = Lens.lens (\ExportLens' {lensAlias} -> lensAlias) (\s@ExportLens' {} a -> s {lensAlias = a} :: ExportLens)

instance Core.AWSRequest ExportLens where
  type AWSResponse ExportLens = ExportLensResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportLensResponse'
            Prelude.<$> (x Data..?> "LensJSON")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportLens where
  hashWithSalt _salt ExportLens' {..} =
    _salt
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData ExportLens where
  rnf ExportLens' {..} =
    Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf lensAlias

instance Data.ToHeaders ExportLens where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ExportLens where
  toPath ExportLens' {..} =
    Prelude.mconcat
      ["/lenses/", Data.toBS lensAlias, "/export"]

instance Data.ToQuery ExportLens where
  toQuery ExportLens' {..} =
    Prelude.mconcat ["LensVersion" Data.=: lensVersion]

-- | /See:/ 'newExportLensResponse' smart constructor.
data ExportLensResponse = ExportLensResponse'
  { -- | The JSON for the lens.
    lensJSON :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportLensResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensJSON', 'exportLensResponse_lensJSON' - The JSON for the lens.
--
-- 'httpStatus', 'exportLensResponse_httpStatus' - The response's http status code.
newExportLensResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportLensResponse
newExportLensResponse pHttpStatus_ =
  ExportLensResponse'
    { lensJSON = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON for the lens.
exportLensResponse_lensJSON :: Lens.Lens' ExportLensResponse (Prelude.Maybe Prelude.Text)
exportLensResponse_lensJSON = Lens.lens (\ExportLensResponse' {lensJSON} -> lensJSON) (\s@ExportLensResponse' {} a -> s {lensJSON = a} :: ExportLensResponse)

-- | The response's http status code.
exportLensResponse_httpStatus :: Lens.Lens' ExportLensResponse Prelude.Int
exportLensResponse_httpStatus = Lens.lens (\ExportLensResponse' {httpStatus} -> httpStatus) (\s@ExportLensResponse' {} a -> s {httpStatus = a} :: ExportLensResponse)

instance Prelude.NFData ExportLensResponse where
  rnf ExportLensResponse' {..} =
    Prelude.rnf lensJSON
      `Prelude.seq` Prelude.rnf httpStatus
