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
-- Module      : Amazonka.AppStream.StopImageBuilder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified image builder.
module Amazonka.AppStream.StopImageBuilder
  ( -- * Creating a Request
    StopImageBuilder (..),
    newStopImageBuilder,

    -- * Request Lenses
    stopImageBuilder_name,

    -- * Destructuring the Response
    StopImageBuilderResponse (..),
    newStopImageBuilderResponse,

    -- * Response Lenses
    stopImageBuilderResponse_imageBuilder,
    stopImageBuilderResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopImageBuilder' smart constructor.
data StopImageBuilder = StopImageBuilder'
  { -- | The name of the image builder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopImageBuilder_name' - The name of the image builder.
newStopImageBuilder ::
  -- | 'name'
  Prelude.Text ->
  StopImageBuilder
newStopImageBuilder pName_ =
  StopImageBuilder' {name = pName_}

-- | The name of the image builder.
stopImageBuilder_name :: Lens.Lens' StopImageBuilder Prelude.Text
stopImageBuilder_name = Lens.lens (\StopImageBuilder' {name} -> name) (\s@StopImageBuilder' {} a -> s {name = a} :: StopImageBuilder)

instance Core.AWSRequest StopImageBuilder where
  type
    AWSResponse StopImageBuilder =
      StopImageBuilderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopImageBuilderResponse'
            Prelude.<$> (x Data..?> "ImageBuilder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopImageBuilder where
  hashWithSalt _salt StopImageBuilder' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StopImageBuilder where
  rnf StopImageBuilder' {..} = Prelude.rnf name

instance Data.ToHeaders StopImageBuilder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.StopImageBuilder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopImageBuilder where
  toJSON StopImageBuilder' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath StopImageBuilder where
  toPath = Prelude.const "/"

instance Data.ToQuery StopImageBuilder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopImageBuilderResponse' smart constructor.
data StopImageBuilderResponse = StopImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Prelude.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'stopImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'stopImageBuilderResponse_httpStatus' - The response's http status code.
newStopImageBuilderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopImageBuilderResponse
newStopImageBuilderResponse pHttpStatus_ =
  StopImageBuilderResponse'
    { imageBuilder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
stopImageBuilderResponse_imageBuilder :: Lens.Lens' StopImageBuilderResponse (Prelude.Maybe ImageBuilder)
stopImageBuilderResponse_imageBuilder = Lens.lens (\StopImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@StopImageBuilderResponse' {} a -> s {imageBuilder = a} :: StopImageBuilderResponse)

-- | The response's http status code.
stopImageBuilderResponse_httpStatus :: Lens.Lens' StopImageBuilderResponse Prelude.Int
stopImageBuilderResponse_httpStatus = Lens.lens (\StopImageBuilderResponse' {httpStatus} -> httpStatus) (\s@StopImageBuilderResponse' {} a -> s {httpStatus = a} :: StopImageBuilderResponse)

instance Prelude.NFData StopImageBuilderResponse where
  rnf StopImageBuilderResponse' {..} =
    Prelude.rnf imageBuilder
      `Prelude.seq` Prelude.rnf httpStatus
