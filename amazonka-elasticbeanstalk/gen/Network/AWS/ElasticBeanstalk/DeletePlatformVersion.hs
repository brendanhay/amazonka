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
-- Module      : Network.AWS.ElasticBeanstalk.DeletePlatformVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of a custom platform.
module Network.AWS.ElasticBeanstalk.DeletePlatformVersion
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

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePlatformVersion' smart constructor.
data DeletePlatformVersion = DeletePlatformVersion'
  { -- | The ARN of the version of the custom platform.
    platformArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeletePlatformVersion where
  type
    Rs DeletePlatformVersion =
      DeletePlatformVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeletePlatformVersionResult"
      ( \s h x ->
          DeletePlatformVersionResponse'
            Prelude.<$> (x Prelude..@? "PlatformSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePlatformVersion

instance Prelude.NFData DeletePlatformVersion

instance Prelude.ToHeaders DeletePlatformVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePlatformVersion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePlatformVersion where
  toQuery DeletePlatformVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeletePlatformVersion" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "PlatformArn" Prelude.=: platformArn
      ]

-- | /See:/ 'newDeletePlatformVersionResponse' smart constructor.
data DeletePlatformVersionResponse = DeletePlatformVersionResponse'
  { -- | Detailed information about the version of the custom platform.
    platformSummary :: Prelude.Maybe PlatformSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeletePlatformVersionResponse
