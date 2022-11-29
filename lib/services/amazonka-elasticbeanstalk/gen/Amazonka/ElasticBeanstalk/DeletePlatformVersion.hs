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
-- Module      : Amazonka.ElasticBeanstalk.DeletePlatformVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of a custom platform.
module Amazonka.ElasticBeanstalk.DeletePlatformVersion
  ( -- * Creating a Request
    DeletePlatformVersion (..),
    newDeletePlatformVersion,

    -- * Request Lenses
    deletePlatformVersion_platformArn,

    -- * Destructuring the Response
    DeletePlatformVersionResponse (..),
    newDeletePlatformVersionResponse,

    -- * Response Lenses
    deletePlatformVersionResponse_platformSummary,
    deletePlatformVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePlatformVersion' smart constructor.
data DeletePlatformVersion = DeletePlatformVersion'
  { -- | The ARN of the version of the custom platform.
    platformArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlatformVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformArn', 'deletePlatformVersion_platformArn' - The ARN of the version of the custom platform.
newDeletePlatformVersion ::
  DeletePlatformVersion
newDeletePlatformVersion =
  DeletePlatformVersion'
    { platformArn =
        Prelude.Nothing
    }

-- | The ARN of the version of the custom platform.
deletePlatformVersion_platformArn :: Lens.Lens' DeletePlatformVersion (Prelude.Maybe Prelude.Text)
deletePlatformVersion_platformArn = Lens.lens (\DeletePlatformVersion' {platformArn} -> platformArn) (\s@DeletePlatformVersion' {} a -> s {platformArn = a} :: DeletePlatformVersion)

instance Core.AWSRequest DeletePlatformVersion where
  type
    AWSResponse DeletePlatformVersion =
      DeletePlatformVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeletePlatformVersionResult"
      ( \s h x ->
          DeletePlatformVersionResponse'
            Prelude.<$> (x Core..@? "PlatformSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePlatformVersion where
  hashWithSalt _salt DeletePlatformVersion' {..} =
    _salt `Prelude.hashWithSalt` platformArn

instance Prelude.NFData DeletePlatformVersion where
  rnf DeletePlatformVersion' {..} =
    Prelude.rnf platformArn

instance Core.ToHeaders DeletePlatformVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeletePlatformVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery DeletePlatformVersion where
  toQuery DeletePlatformVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeletePlatformVersion" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "PlatformArn" Core.=: platformArn
      ]

-- | /See:/ 'newDeletePlatformVersionResponse' smart constructor.
data DeletePlatformVersionResponse = DeletePlatformVersionResponse'
  { -- | Detailed information about the version of the custom platform.
    platformSummary :: Prelude.Maybe PlatformSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlatformVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformSummary', 'deletePlatformVersionResponse_platformSummary' - Detailed information about the version of the custom platform.
--
-- 'httpStatus', 'deletePlatformVersionResponse_httpStatus' - The response's http status code.
newDeletePlatformVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePlatformVersionResponse
newDeletePlatformVersionResponse pHttpStatus_ =
  DeletePlatformVersionResponse'
    { platformSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the version of the custom platform.
deletePlatformVersionResponse_platformSummary :: Lens.Lens' DeletePlatformVersionResponse (Prelude.Maybe PlatformSummary)
deletePlatformVersionResponse_platformSummary = Lens.lens (\DeletePlatformVersionResponse' {platformSummary} -> platformSummary) (\s@DeletePlatformVersionResponse' {} a -> s {platformSummary = a} :: DeletePlatformVersionResponse)

-- | The response's http status code.
deletePlatformVersionResponse_httpStatus :: Lens.Lens' DeletePlatformVersionResponse Prelude.Int
deletePlatformVersionResponse_httpStatus = Lens.lens (\DeletePlatformVersionResponse' {httpStatus} -> httpStatus) (\s@DeletePlatformVersionResponse' {} a -> s {httpStatus = a} :: DeletePlatformVersionResponse)

instance Prelude.NFData DeletePlatformVersionResponse where
  rnf DeletePlatformVersionResponse' {..} =
    Prelude.rnf platformSummary
      `Prelude.seq` Prelude.rnf httpStatus
