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
-- Module      : Amazonka.AppStream.CreateImageBuilderStreamingURL
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an image builder streaming session.
module Amazonka.AppStream.CreateImageBuilderStreamingURL
  ( -- * Creating a Request
    CreateImageBuilderStreamingURL (..),
    newCreateImageBuilderStreamingURL,

    -- * Request Lenses
    createImageBuilderStreamingURL_validity,
    createImageBuilderStreamingURL_name,

    -- * Destructuring the Response
    CreateImageBuilderStreamingURLResponse (..),
    newCreateImageBuilderStreamingURLResponse,

    -- * Response Lenses
    createImageBuilderStreamingURLResponse_expires,
    createImageBuilderStreamingURLResponse_streamingURL,
    createImageBuilderStreamingURLResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateImageBuilderStreamingURL' smart constructor.
data CreateImageBuilderStreamingURL = CreateImageBuilderStreamingURL'
  { -- | The time that the streaming URL will be valid, in seconds. Specify a
    -- value between 1 and 604800 seconds. The default is 3600 seconds.
    validity :: Prelude.Maybe Prelude.Integer,
    -- | The name of the image builder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageBuilderStreamingURL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validity', 'createImageBuilderStreamingURL_validity' - The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 3600 seconds.
--
-- 'name', 'createImageBuilderStreamingURL_name' - The name of the image builder.
newCreateImageBuilderStreamingURL ::
  -- | 'name'
  Prelude.Text ->
  CreateImageBuilderStreamingURL
newCreateImageBuilderStreamingURL pName_ =
  CreateImageBuilderStreamingURL'
    { validity =
        Prelude.Nothing,
      name = pName_
    }

-- | The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 3600 seconds.
createImageBuilderStreamingURL_validity :: Lens.Lens' CreateImageBuilderStreamingURL (Prelude.Maybe Prelude.Integer)
createImageBuilderStreamingURL_validity = Lens.lens (\CreateImageBuilderStreamingURL' {validity} -> validity) (\s@CreateImageBuilderStreamingURL' {} a -> s {validity = a} :: CreateImageBuilderStreamingURL)

-- | The name of the image builder.
createImageBuilderStreamingURL_name :: Lens.Lens' CreateImageBuilderStreamingURL Prelude.Text
createImageBuilderStreamingURL_name = Lens.lens (\CreateImageBuilderStreamingURL' {name} -> name) (\s@CreateImageBuilderStreamingURL' {} a -> s {name = a} :: CreateImageBuilderStreamingURL)

instance
  Core.AWSRequest
    CreateImageBuilderStreamingURL
  where
  type
    AWSResponse CreateImageBuilderStreamingURL =
      CreateImageBuilderStreamingURLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageBuilderStreamingURLResponse'
            Prelude.<$> (x Data..?> "Expires")
            Prelude.<*> (x Data..?> "StreamingURL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateImageBuilderStreamingURL
  where
  hashWithSalt
    _salt
    CreateImageBuilderStreamingURL' {..} =
      _salt `Prelude.hashWithSalt` validity
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    CreateImageBuilderStreamingURL
  where
  rnf CreateImageBuilderStreamingURL' {..} =
    Prelude.rnf validity `Prelude.seq` Prelude.rnf name

instance
  Data.ToHeaders
    CreateImageBuilderStreamingURL
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateImageBuilderStreamingURL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateImageBuilderStreamingURL where
  toJSON CreateImageBuilderStreamingURL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Validity" Data..=) Prelude.<$> validity,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateImageBuilderStreamingURL where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateImageBuilderStreamingURL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImageBuilderStreamingURLResponse' smart constructor.
data CreateImageBuilderStreamingURLResponse = CreateImageBuilderStreamingURLResponse'
  { -- | The elapsed time, in seconds after the Unix epoch, when this URL
    -- expires.
    expires :: Prelude.Maybe Data.POSIX,
    -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageBuilderStreamingURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expires', 'createImageBuilderStreamingURLResponse_expires' - The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
--
-- 'streamingURL', 'createImageBuilderStreamingURLResponse_streamingURL' - The URL to start the AppStream 2.0 streaming session.
--
-- 'httpStatus', 'createImageBuilderStreamingURLResponse_httpStatus' - The response's http status code.
newCreateImageBuilderStreamingURLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImageBuilderStreamingURLResponse
newCreateImageBuilderStreamingURLResponse
  pHttpStatus_ =
    CreateImageBuilderStreamingURLResponse'
      { expires =
          Prelude.Nothing,
        streamingURL = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
createImageBuilderStreamingURLResponse_expires :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Prelude.Maybe Prelude.UTCTime)
createImageBuilderStreamingURLResponse_expires = Lens.lens (\CreateImageBuilderStreamingURLResponse' {expires} -> expires) (\s@CreateImageBuilderStreamingURLResponse' {} a -> s {expires = a} :: CreateImageBuilderStreamingURLResponse) Prelude.. Lens.mapping Data._Time

-- | The URL to start the AppStream 2.0 streaming session.
createImageBuilderStreamingURLResponse_streamingURL :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Prelude.Maybe Prelude.Text)
createImageBuilderStreamingURLResponse_streamingURL = Lens.lens (\CreateImageBuilderStreamingURLResponse' {streamingURL} -> streamingURL) (\s@CreateImageBuilderStreamingURLResponse' {} a -> s {streamingURL = a} :: CreateImageBuilderStreamingURLResponse)

-- | The response's http status code.
createImageBuilderStreamingURLResponse_httpStatus :: Lens.Lens' CreateImageBuilderStreamingURLResponse Prelude.Int
createImageBuilderStreamingURLResponse_httpStatus = Lens.lens (\CreateImageBuilderStreamingURLResponse' {httpStatus} -> httpStatus) (\s@CreateImageBuilderStreamingURLResponse' {} a -> s {httpStatus = a} :: CreateImageBuilderStreamingURLResponse)

instance
  Prelude.NFData
    CreateImageBuilderStreamingURLResponse
  where
  rnf CreateImageBuilderStreamingURLResponse' {..} =
    Prelude.rnf expires
      `Prelude.seq` Prelude.rnf streamingURL
      `Prelude.seq` Prelude.rnf httpStatus
