{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.StartImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified image builder.
module Network.AWS.AppStream.StartImageBuilder
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartImageBuilder' smart constructor.
data StartImageBuilder = StartImageBuilder'
  { -- | The version of the AppStream 2.0 agent to use for this image builder. To
    -- use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the image builder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StartImageBuilder where
  type Rs StartImageBuilder = StartImageBuilderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageBuilderResponse'
            Prelude.<$> (x Prelude..?> "ImageBuilder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImageBuilder

instance Prelude.NFData StartImageBuilder

instance Prelude.ToHeaders StartImageBuilder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.StartImageBuilder" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartImageBuilder where
  toJSON StartImageBuilder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AppstreamAgentVersion" Prelude..=)
              Prelude.<$> appstreamAgentVersion,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath StartImageBuilder where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartImageBuilder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImageBuilderResponse' smart constructor.
data StartImageBuilderResponse = StartImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Prelude.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StartImageBuilderResponse
