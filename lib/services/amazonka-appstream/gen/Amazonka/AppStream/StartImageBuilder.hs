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
-- Module      : Amazonka.AppStream.StartImageBuilder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified image builder.
module Amazonka.AppStream.StartImageBuilder
  ( -- * Creating a Request
    StartImageBuilder (..),
    newStartImageBuilder,

    -- * Request Lenses
    startImageBuilder_appstreamAgentVersion,
    startImageBuilder_name,

    -- * Destructuring the Response
    StartImageBuilderResponse (..),
    newStartImageBuilderResponse,

    -- * Response Lenses
    startImageBuilderResponse_imageBuilder,
    startImageBuilderResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartImageBuilder' smart constructor.
data StartImageBuilder = StartImageBuilder'
  { -- | The version of the AppStream 2.0 agent to use for this image builder. To
    -- use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the image builder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appstreamAgentVersion', 'startImageBuilder_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- 'name', 'startImageBuilder_name' - The name of the image builder.
newStartImageBuilder ::
  -- | 'name'
  Prelude.Text ->
  StartImageBuilder
newStartImageBuilder pName_ =
  StartImageBuilder'
    { appstreamAgentVersion =
        Prelude.Nothing,
      name = pName_
    }

-- | The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
startImageBuilder_appstreamAgentVersion :: Lens.Lens' StartImageBuilder (Prelude.Maybe Prelude.Text)
startImageBuilder_appstreamAgentVersion = Lens.lens (\StartImageBuilder' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@StartImageBuilder' {} a -> s {appstreamAgentVersion = a} :: StartImageBuilder)

-- | The name of the image builder.
startImageBuilder_name :: Lens.Lens' StartImageBuilder Prelude.Text
startImageBuilder_name = Lens.lens (\StartImageBuilder' {name} -> name) (\s@StartImageBuilder' {} a -> s {name = a} :: StartImageBuilder)

instance Core.AWSRequest StartImageBuilder where
  type
    AWSResponse StartImageBuilder =
      StartImageBuilderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageBuilderResponse'
            Prelude.<$> (x Data..?> "ImageBuilder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImageBuilder where
  hashWithSalt _salt StartImageBuilder' {..} =
    _salt `Prelude.hashWithSalt` appstreamAgentVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartImageBuilder where
  rnf StartImageBuilder' {..} =
    Prelude.rnf appstreamAgentVersion
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders StartImageBuilder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.StartImageBuilder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartImageBuilder where
  toJSON StartImageBuilder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppstreamAgentVersion" Data..=)
              Prelude.<$> appstreamAgentVersion,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath StartImageBuilder where
  toPath = Prelude.const "/"

instance Data.ToQuery StartImageBuilder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImageBuilderResponse' smart constructor.
data StartImageBuilderResponse = StartImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Prelude.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'startImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'startImageBuilderResponse_httpStatus' - The response's http status code.
newStartImageBuilderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImageBuilderResponse
newStartImageBuilderResponse pHttpStatus_ =
  StartImageBuilderResponse'
    { imageBuilder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
startImageBuilderResponse_imageBuilder :: Lens.Lens' StartImageBuilderResponse (Prelude.Maybe ImageBuilder)
startImageBuilderResponse_imageBuilder = Lens.lens (\StartImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@StartImageBuilderResponse' {} a -> s {imageBuilder = a} :: StartImageBuilderResponse)

-- | The response's http status code.
startImageBuilderResponse_httpStatus :: Lens.Lens' StartImageBuilderResponse Prelude.Int
startImageBuilderResponse_httpStatus = Lens.lens (\StartImageBuilderResponse' {httpStatus} -> httpStatus) (\s@StartImageBuilderResponse' {} a -> s {httpStatus = a} :: StartImageBuilderResponse)

instance Prelude.NFData StartImageBuilderResponse where
  rnf StartImageBuilderResponse' {..} =
    Prelude.rnf imageBuilder
      `Prelude.seq` Prelude.rnf httpStatus
