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
-- Module      : Amazonka.DrS.CreateExtendedSourceServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an extended source server in the target Account based on the
-- source server in staging account.
module Amazonka.DrS.CreateExtendedSourceServer
  ( -- * Creating a Request
    CreateExtendedSourceServer (..),
    newCreateExtendedSourceServer,

    -- * Request Lenses
    createExtendedSourceServer_tags,
    createExtendedSourceServer_sourceServerArn,

    -- * Destructuring the Response
    CreateExtendedSourceServerResponse (..),
    newCreateExtendedSourceServerResponse,

    -- * Response Lenses
    createExtendedSourceServerResponse_sourceServer,
    createExtendedSourceServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExtendedSourceServer' smart constructor.
data CreateExtendedSourceServer = CreateExtendedSourceServer'
  { -- | A list of tags associated with the extended source server.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | This defines the ARN of the source server in staging Account based on
    -- which you want to create an extended source server.
    sourceServerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExtendedSourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createExtendedSourceServer_tags' - A list of tags associated with the extended source server.
--
-- 'sourceServerArn', 'createExtendedSourceServer_sourceServerArn' - This defines the ARN of the source server in staging Account based on
-- which you want to create an extended source server.
newCreateExtendedSourceServer ::
  -- | 'sourceServerArn'
  Prelude.Text ->
  CreateExtendedSourceServer
newCreateExtendedSourceServer pSourceServerArn_ =
  CreateExtendedSourceServer'
    { tags = Prelude.Nothing,
      sourceServerArn = pSourceServerArn_
    }

-- | A list of tags associated with the extended source server.
createExtendedSourceServer_tags :: Lens.Lens' CreateExtendedSourceServer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExtendedSourceServer_tags = Lens.lens (\CreateExtendedSourceServer' {tags} -> tags) (\s@CreateExtendedSourceServer' {} a -> s {tags = a} :: CreateExtendedSourceServer) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | This defines the ARN of the source server in staging Account based on
-- which you want to create an extended source server.
createExtendedSourceServer_sourceServerArn :: Lens.Lens' CreateExtendedSourceServer Prelude.Text
createExtendedSourceServer_sourceServerArn = Lens.lens (\CreateExtendedSourceServer' {sourceServerArn} -> sourceServerArn) (\s@CreateExtendedSourceServer' {} a -> s {sourceServerArn = a} :: CreateExtendedSourceServer)

instance Core.AWSRequest CreateExtendedSourceServer where
  type
    AWSResponse CreateExtendedSourceServer =
      CreateExtendedSourceServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExtendedSourceServerResponse'
            Prelude.<$> (x Core..?> "sourceServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExtendedSourceServer where
  hashWithSalt _salt CreateExtendedSourceServer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceServerArn

instance Prelude.NFData CreateExtendedSourceServer where
  rnf CreateExtendedSourceServer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceServerArn

instance Core.ToHeaders CreateExtendedSourceServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateExtendedSourceServer where
  toJSON CreateExtendedSourceServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("sourceServerArn" Core..= sourceServerArn)
          ]
      )

instance Core.ToPath CreateExtendedSourceServer where
  toPath = Prelude.const "/CreateExtendedSourceServer"

instance Core.ToQuery CreateExtendedSourceServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExtendedSourceServerResponse' smart constructor.
data CreateExtendedSourceServerResponse = CreateExtendedSourceServerResponse'
  { -- | Created extended source server.
    sourceServer :: Prelude.Maybe SourceServer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExtendedSourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServer', 'createExtendedSourceServerResponse_sourceServer' - Created extended source server.
--
-- 'httpStatus', 'createExtendedSourceServerResponse_httpStatus' - The response's http status code.
newCreateExtendedSourceServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateExtendedSourceServerResponse
newCreateExtendedSourceServerResponse pHttpStatus_ =
  CreateExtendedSourceServerResponse'
    { sourceServer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Created extended source server.
createExtendedSourceServerResponse_sourceServer :: Lens.Lens' CreateExtendedSourceServerResponse (Prelude.Maybe SourceServer)
createExtendedSourceServerResponse_sourceServer = Lens.lens (\CreateExtendedSourceServerResponse' {sourceServer} -> sourceServer) (\s@CreateExtendedSourceServerResponse' {} a -> s {sourceServer = a} :: CreateExtendedSourceServerResponse)

-- | The response's http status code.
createExtendedSourceServerResponse_httpStatus :: Lens.Lens' CreateExtendedSourceServerResponse Prelude.Int
createExtendedSourceServerResponse_httpStatus = Lens.lens (\CreateExtendedSourceServerResponse' {httpStatus} -> httpStatus) (\s@CreateExtendedSourceServerResponse' {} a -> s {httpStatus = a} :: CreateExtendedSourceServerResponse)

instance
  Prelude.NFData
    CreateExtendedSourceServerResponse
  where
  rnf CreateExtendedSourceServerResponse' {..} =
    Prelude.rnf sourceServer
      `Prelude.seq` Prelude.rnf httpStatus
