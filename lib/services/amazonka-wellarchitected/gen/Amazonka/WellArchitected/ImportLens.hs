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
-- Module      : Amazonka.WellArchitected.ImportLens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import a new lens.
--
-- The lens cannot be applied to workloads or shared with other Amazon Web
-- Services accounts until it\'s published with CreateLensVersion
--
-- Lenses are defined in JSON. For more information, see
-- <https://docs.aws.amazon.com/wellarchitected/latest/userguide/lenses-format-specification.html JSON format specification>
-- in the /Well-Architected Tool User Guide/.
--
-- A custom lens cannot exceed 500 KB in size.
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
module Amazonka.WellArchitected.ImportLens
  ( -- * Creating a Request
    ImportLens (..),
    newImportLens,

    -- * Request Lenses
    importLens_lensAlias,
    importLens_tags,
    importLens_jSONString,
    importLens_clientRequestToken,

    -- * Destructuring the Response
    ImportLensResponse (..),
    newImportLensResponse,

    -- * Response Lenses
    importLensResponse_lensArn,
    importLensResponse_status,
    importLensResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newImportLens' smart constructor.
data ImportLens = ImportLens'
  { lensAlias :: Prelude.Maybe Prelude.Text,
    -- | Tags to associate to a lens.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The JSON representation of a lens.
    jSONString :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportLens' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensAlias', 'importLens_lensAlias' - Undocumented member.
--
-- 'tags', 'importLens_tags' - Tags to associate to a lens.
--
-- 'jSONString', 'importLens_jSONString' - The JSON representation of a lens.
--
-- 'clientRequestToken', 'importLens_clientRequestToken' - Undocumented member.
newImportLens ::
  -- | 'jSONString'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  ImportLens
newImportLens pJSONString_ pClientRequestToken_ =
  ImportLens'
    { lensAlias = Prelude.Nothing,
      tags = Prelude.Nothing,
      jSONString = pJSONString_,
      clientRequestToken = pClientRequestToken_
    }

-- | Undocumented member.
importLens_lensAlias :: Lens.Lens' ImportLens (Prelude.Maybe Prelude.Text)
importLens_lensAlias = Lens.lens (\ImportLens' {lensAlias} -> lensAlias) (\s@ImportLens' {} a -> s {lensAlias = a} :: ImportLens)

-- | Tags to associate to a lens.
importLens_tags :: Lens.Lens' ImportLens (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importLens_tags = Lens.lens (\ImportLens' {tags} -> tags) (\s@ImportLens' {} a -> s {tags = a} :: ImportLens) Prelude.. Lens.mapping Lens.coerced

-- | The JSON representation of a lens.
importLens_jSONString :: Lens.Lens' ImportLens Prelude.Text
importLens_jSONString = Lens.lens (\ImportLens' {jSONString} -> jSONString) (\s@ImportLens' {} a -> s {jSONString = a} :: ImportLens)

-- | Undocumented member.
importLens_clientRequestToken :: Lens.Lens' ImportLens Prelude.Text
importLens_clientRequestToken = Lens.lens (\ImportLens' {clientRequestToken} -> clientRequestToken) (\s@ImportLens' {} a -> s {clientRequestToken = a} :: ImportLens)

instance Core.AWSRequest ImportLens where
  type AWSResponse ImportLens = ImportLensResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportLensResponse'
            Prelude.<$> (x Data..?> "LensArn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportLens where
  hashWithSalt _salt ImportLens' {..} =
    _salt `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jSONString
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData ImportLens where
  rnf ImportLens' {..} =
    Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jSONString
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders ImportLens where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportLens where
  toJSON ImportLens' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LensAlias" Data..=) Prelude.<$> lensAlias,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("JSONString" Data..= jSONString),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath ImportLens where
  toPath = Prelude.const "/importLens"

instance Data.ToQuery ImportLens where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportLensResponse' smart constructor.
data ImportLensResponse = ImportLensResponse'
  { -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the imported lens.
    status :: Prelude.Maybe ImportLensStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportLensResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensArn', 'importLensResponse_lensArn' - The ARN for the lens.
--
-- 'status', 'importLensResponse_status' - The status of the imported lens.
--
-- 'httpStatus', 'importLensResponse_httpStatus' - The response's http status code.
newImportLensResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportLensResponse
newImportLensResponse pHttpStatus_ =
  ImportLensResponse'
    { lensArn = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the lens.
importLensResponse_lensArn :: Lens.Lens' ImportLensResponse (Prelude.Maybe Prelude.Text)
importLensResponse_lensArn = Lens.lens (\ImportLensResponse' {lensArn} -> lensArn) (\s@ImportLensResponse' {} a -> s {lensArn = a} :: ImportLensResponse)

-- | The status of the imported lens.
importLensResponse_status :: Lens.Lens' ImportLensResponse (Prelude.Maybe ImportLensStatus)
importLensResponse_status = Lens.lens (\ImportLensResponse' {status} -> status) (\s@ImportLensResponse' {} a -> s {status = a} :: ImportLensResponse)

-- | The response's http status code.
importLensResponse_httpStatus :: Lens.Lens' ImportLensResponse Prelude.Int
importLensResponse_httpStatus = Lens.lens (\ImportLensResponse' {httpStatus} -> httpStatus) (\s@ImportLensResponse' {} a -> s {httpStatus = a} :: ImportLensResponse)

instance Prelude.NFData ImportLensResponse where
  rnf ImportLensResponse' {..} =
    Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
