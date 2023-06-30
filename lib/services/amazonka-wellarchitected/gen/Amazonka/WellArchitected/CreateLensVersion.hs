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
-- Module      : Amazonka.WellArchitected.CreateLensVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new lens version.
--
-- A lens can have up to 100 versions.
--
-- After a lens has been imported, create a new lens version to publish it.
-- The owner of a lens can share the lens with other Amazon Web Services
-- accounts and IAM users in the same Amazon Web Services Region. Only the
-- owner of a lens can delete it.
module Amazonka.WellArchitected.CreateLensVersion
  ( -- * Creating a Request
    CreateLensVersion (..),
    newCreateLensVersion,

    -- * Request Lenses
    createLensVersion_isMajorVersion,
    createLensVersion_lensAlias,
    createLensVersion_lensVersion,
    createLensVersion_clientRequestToken,

    -- * Destructuring the Response
    CreateLensVersionResponse (..),
    newCreateLensVersionResponse,

    -- * Response Lenses
    createLensVersionResponse_lensArn,
    createLensVersionResponse_lensVersion,
    createLensVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newCreateLensVersion' smart constructor.
data CreateLensVersion = CreateLensVersion'
  { -- | Set to true if this new major lens version.
    isMajorVersion :: Prelude.Maybe Prelude.Bool,
    lensAlias :: Prelude.Text,
    -- | The version of the lens being created.
    lensVersion :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLensVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMajorVersion', 'createLensVersion_isMajorVersion' - Set to true if this new major lens version.
--
-- 'lensAlias', 'createLensVersion_lensAlias' - Undocumented member.
--
-- 'lensVersion', 'createLensVersion_lensVersion' - The version of the lens being created.
--
-- 'clientRequestToken', 'createLensVersion_clientRequestToken' - Undocumented member.
newCreateLensVersion ::
  -- | 'lensAlias'
  Prelude.Text ->
  -- | 'lensVersion'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateLensVersion
newCreateLensVersion
  pLensAlias_
  pLensVersion_
  pClientRequestToken_ =
    CreateLensVersion'
      { isMajorVersion =
          Prelude.Nothing,
        lensAlias = pLensAlias_,
        lensVersion = pLensVersion_,
        clientRequestToken = pClientRequestToken_
      }

-- | Set to true if this new major lens version.
createLensVersion_isMajorVersion :: Lens.Lens' CreateLensVersion (Prelude.Maybe Prelude.Bool)
createLensVersion_isMajorVersion = Lens.lens (\CreateLensVersion' {isMajorVersion} -> isMajorVersion) (\s@CreateLensVersion' {} a -> s {isMajorVersion = a} :: CreateLensVersion)

-- | Undocumented member.
createLensVersion_lensAlias :: Lens.Lens' CreateLensVersion Prelude.Text
createLensVersion_lensAlias = Lens.lens (\CreateLensVersion' {lensAlias} -> lensAlias) (\s@CreateLensVersion' {} a -> s {lensAlias = a} :: CreateLensVersion)

-- | The version of the lens being created.
createLensVersion_lensVersion :: Lens.Lens' CreateLensVersion Prelude.Text
createLensVersion_lensVersion = Lens.lens (\CreateLensVersion' {lensVersion} -> lensVersion) (\s@CreateLensVersion' {} a -> s {lensVersion = a} :: CreateLensVersion)

-- | Undocumented member.
createLensVersion_clientRequestToken :: Lens.Lens' CreateLensVersion Prelude.Text
createLensVersion_clientRequestToken = Lens.lens (\CreateLensVersion' {clientRequestToken} -> clientRequestToken) (\s@CreateLensVersion' {} a -> s {clientRequestToken = a} :: CreateLensVersion)

instance Core.AWSRequest CreateLensVersion where
  type
    AWSResponse CreateLensVersion =
      CreateLensVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLensVersionResponse'
            Prelude.<$> (x Data..?> "LensArn")
            Prelude.<*> (x Data..?> "LensVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLensVersion where
  hashWithSalt _salt CreateLensVersion' {..} =
    _salt
      `Prelude.hashWithSalt` isMajorVersion
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateLensVersion where
  rnf CreateLensVersion' {..} =
    Prelude.rnf isMajorVersion
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateLensVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLensVersion where
  toJSON CreateLensVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsMajorVersion" Data..=)
              Prelude.<$> isMajorVersion,
            Prelude.Just ("LensVersion" Data..= lensVersion),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateLensVersion where
  toPath CreateLensVersion' {..} =
    Prelude.mconcat
      ["/lenses/", Data.toBS lensAlias, "/versions"]

instance Data.ToQuery CreateLensVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLensVersionResponse' smart constructor.
data CreateLensVersionResponse = CreateLensVersionResponse'
  { -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the lens.
    lensVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLensVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensArn', 'createLensVersionResponse_lensArn' - The ARN for the lens.
--
-- 'lensVersion', 'createLensVersionResponse_lensVersion' - The version of the lens.
--
-- 'httpStatus', 'createLensVersionResponse_httpStatus' - The response's http status code.
newCreateLensVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLensVersionResponse
newCreateLensVersionResponse pHttpStatus_ =
  CreateLensVersionResponse'
    { lensArn =
        Prelude.Nothing,
      lensVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the lens.
createLensVersionResponse_lensArn :: Lens.Lens' CreateLensVersionResponse (Prelude.Maybe Prelude.Text)
createLensVersionResponse_lensArn = Lens.lens (\CreateLensVersionResponse' {lensArn} -> lensArn) (\s@CreateLensVersionResponse' {} a -> s {lensArn = a} :: CreateLensVersionResponse)

-- | The version of the lens.
createLensVersionResponse_lensVersion :: Lens.Lens' CreateLensVersionResponse (Prelude.Maybe Prelude.Text)
createLensVersionResponse_lensVersion = Lens.lens (\CreateLensVersionResponse' {lensVersion} -> lensVersion) (\s@CreateLensVersionResponse' {} a -> s {lensVersion = a} :: CreateLensVersionResponse)

-- | The response's http status code.
createLensVersionResponse_httpStatus :: Lens.Lens' CreateLensVersionResponse Prelude.Int
createLensVersionResponse_httpStatus = Lens.lens (\CreateLensVersionResponse' {httpStatus} -> httpStatus) (\s@CreateLensVersionResponse' {} a -> s {httpStatus = a} :: CreateLensVersionResponse)

instance Prelude.NFData CreateLensVersionResponse where
  rnf CreateLensVersionResponse' {..} =
    Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf httpStatus
