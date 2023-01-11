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
-- Module      : Amazonka.Lightsail.GetContainerAPIMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Lightsail containers, such as the
-- current version of the Lightsail Control (lightsailctl) plugin.
module Amazonka.Lightsail.GetContainerAPIMetadata
  ( -- * Creating a Request
    GetContainerAPIMetadata (..),
    newGetContainerAPIMetadata,

    -- * Destructuring the Response
    GetContainerAPIMetadataResponse (..),
    newGetContainerAPIMetadataResponse,

    -- * Response Lenses
    getContainerAPIMetadataResponse_metadata,
    getContainerAPIMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerAPIMetadata' smart constructor.
data GetContainerAPIMetadata = GetContainerAPIMetadata'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerAPIMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetContainerAPIMetadata ::
  GetContainerAPIMetadata
newGetContainerAPIMetadata = GetContainerAPIMetadata'

instance Core.AWSRequest GetContainerAPIMetadata where
  type
    AWSResponse GetContainerAPIMetadata =
      GetContainerAPIMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerAPIMetadataResponse'
            Prelude.<$> (x Data..?> "metadata" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerAPIMetadata where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetContainerAPIMetadata where
  rnf _ = ()

instance Data.ToHeaders GetContainerAPIMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetContainerAPIMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContainerAPIMetadata where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetContainerAPIMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContainerAPIMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerAPIMetadataResponse' smart constructor.
data GetContainerAPIMetadataResponse = GetContainerAPIMetadataResponse'
  { -- | Metadata about Lightsail containers, such as the current version of the
    -- Lightsail Control (lightsailctl) plugin.
    metadata :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerAPIMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getContainerAPIMetadataResponse_metadata' - Metadata about Lightsail containers, such as the current version of the
-- Lightsail Control (lightsailctl) plugin.
--
-- 'httpStatus', 'getContainerAPIMetadataResponse_httpStatus' - The response's http status code.
newGetContainerAPIMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContainerAPIMetadataResponse
newGetContainerAPIMetadataResponse pHttpStatus_ =
  GetContainerAPIMetadataResponse'
    { metadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata about Lightsail containers, such as the current version of the
-- Lightsail Control (lightsailctl) plugin.
getContainerAPIMetadataResponse_metadata :: Lens.Lens' GetContainerAPIMetadataResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
getContainerAPIMetadataResponse_metadata = Lens.lens (\GetContainerAPIMetadataResponse' {metadata} -> metadata) (\s@GetContainerAPIMetadataResponse' {} a -> s {metadata = a} :: GetContainerAPIMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getContainerAPIMetadataResponse_httpStatus :: Lens.Lens' GetContainerAPIMetadataResponse Prelude.Int
getContainerAPIMetadataResponse_httpStatus = Lens.lens (\GetContainerAPIMetadataResponse' {httpStatus} -> httpStatus) (\s@GetContainerAPIMetadataResponse' {} a -> s {httpStatus = a} :: GetContainerAPIMetadataResponse)

instance
  Prelude.NFData
    GetContainerAPIMetadataResponse
  where
  rnf GetContainerAPIMetadataResponse' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf httpStatus
