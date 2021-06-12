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
-- Module      : Network.AWS.Lightsail.GetContainerAPIMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Lightsail containers, such as the
-- current version of the Lightsail Control (lightsailctl) plugin.
module Network.AWS.Lightsail.GetContainerAPIMetadata
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContainerAPIMetadata' smart constructor.
data GetContainerAPIMetadata = GetContainerAPIMetadata'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerAPIMetadataResponse'
            Core.<$> (x Core..?> "metadata" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetContainerAPIMetadata

instance Core.NFData GetContainerAPIMetadata

instance Core.ToHeaders GetContainerAPIMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContainerAPIMetadata" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetContainerAPIMetadata where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetContainerAPIMetadata where
  toPath = Core.const "/"

instance Core.ToQuery GetContainerAPIMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetContainerAPIMetadataResponse' smart constructor.
data GetContainerAPIMetadataResponse = GetContainerAPIMetadataResponse'
  { -- | Metadata about Lightsail containers, such as the current version of the
    -- Lightsail Control (lightsailctl) plugin.
    metadata :: Core.Maybe [Core.HashMap Core.Text Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetContainerAPIMetadataResponse
newGetContainerAPIMetadataResponse pHttpStatus_ =
  GetContainerAPIMetadataResponse'
    { metadata =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata about Lightsail containers, such as the current version of the
-- Lightsail Control (lightsailctl) plugin.
getContainerAPIMetadataResponse_metadata :: Lens.Lens' GetContainerAPIMetadataResponse (Core.Maybe [Core.HashMap Core.Text Core.Text])
getContainerAPIMetadataResponse_metadata = Lens.lens (\GetContainerAPIMetadataResponse' {metadata} -> metadata) (\s@GetContainerAPIMetadataResponse' {} a -> s {metadata = a} :: GetContainerAPIMetadataResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContainerAPIMetadataResponse_httpStatus :: Lens.Lens' GetContainerAPIMetadataResponse Core.Int
getContainerAPIMetadataResponse_httpStatus = Lens.lens (\GetContainerAPIMetadataResponse' {httpStatus} -> httpStatus) (\s@GetContainerAPIMetadataResponse' {} a -> s {httpStatus = a} :: GetContainerAPIMetadataResponse)

instance Core.NFData GetContainerAPIMetadataResponse
