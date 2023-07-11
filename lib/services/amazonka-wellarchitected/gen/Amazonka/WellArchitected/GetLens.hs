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
-- Module      : Amazonka.WellArchitected.GetLens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an existing lens.
module Amazonka.WellArchitected.GetLens
  ( -- * Creating a Request
    GetLens (..),
    newGetLens,

    -- * Request Lenses
    getLens_lensVersion,
    getLens_lensAlias,

    -- * Destructuring the Response
    GetLensResponse (..),
    newGetLensResponse,

    -- * Response Lenses
    getLensResponse_lens,
    getLensResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newGetLens' smart constructor.
data GetLens = GetLens'
  { -- | The lens version to be retrieved.
    lensVersion :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLens' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensVersion', 'getLens_lensVersion' - The lens version to be retrieved.
--
-- 'lensAlias', 'getLens_lensAlias' - Undocumented member.
newGetLens ::
  -- | 'lensAlias'
  Prelude.Text ->
  GetLens
newGetLens pLensAlias_ =
  GetLens'
    { lensVersion = Prelude.Nothing,
      lensAlias = pLensAlias_
    }

-- | The lens version to be retrieved.
getLens_lensVersion :: Lens.Lens' GetLens (Prelude.Maybe Prelude.Text)
getLens_lensVersion = Lens.lens (\GetLens' {lensVersion} -> lensVersion) (\s@GetLens' {} a -> s {lensVersion = a} :: GetLens)

-- | Undocumented member.
getLens_lensAlias :: Lens.Lens' GetLens Prelude.Text
getLens_lensAlias = Lens.lens (\GetLens' {lensAlias} -> lensAlias) (\s@GetLens' {} a -> s {lensAlias = a} :: GetLens)

instance Core.AWSRequest GetLens where
  type AWSResponse GetLens = GetLensResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLensResponse'
            Prelude.<$> (x Data..?> "Lens")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLens where
  hashWithSalt _salt GetLens' {..} =
    _salt
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData GetLens where
  rnf GetLens' {..} =
    Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf lensAlias

instance Data.ToHeaders GetLens where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLens where
  toPath GetLens' {..} =
    Prelude.mconcat ["/lenses/", Data.toBS lensAlias]

instance Data.ToQuery GetLens where
  toQuery GetLens' {..} =
    Prelude.mconcat ["LensVersion" Data.=: lensVersion]

-- | /See:/ 'newGetLensResponse' smart constructor.
data GetLensResponse = GetLensResponse'
  { -- | A lens return object.
    lens :: Prelude.Maybe Lens,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lens', 'getLensResponse_lens' - A lens return object.
--
-- 'httpStatus', 'getLensResponse_httpStatus' - The response's http status code.
newGetLensResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLensResponse
newGetLensResponse pHttpStatus_ =
  GetLensResponse'
    { lens = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A lens return object.
getLensResponse_lens :: Lens.Lens' GetLensResponse (Prelude.Maybe Lens)
getLensResponse_lens = Lens.lens (\GetLensResponse' {lens} -> lens) (\s@GetLensResponse' {} a -> s {lens = a} :: GetLensResponse)

-- | The response's http status code.
getLensResponse_httpStatus :: Lens.Lens' GetLensResponse Prelude.Int
getLensResponse_httpStatus = Lens.lens (\GetLensResponse' {httpStatus} -> httpStatus) (\s@GetLensResponse' {} a -> s {httpStatus = a} :: GetLensResponse)

instance Prelude.NFData GetLensResponse where
  rnf GetLensResponse' {..} =
    Prelude.rnf lens
      `Prelude.seq` Prelude.rnf httpStatus
