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
-- Module      : Amazonka.CloudFront.GetOriginAccessControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a CloudFront origin access control, including its unique
-- identifier.
module Amazonka.CloudFront.GetOriginAccessControl
  ( -- * Creating a Request
    GetOriginAccessControl (..),
    newGetOriginAccessControl,

    -- * Request Lenses
    getOriginAccessControl_id,

    -- * Destructuring the Response
    GetOriginAccessControlResponse (..),
    newGetOriginAccessControlResponse,

    -- * Response Lenses
    getOriginAccessControlResponse_eTag,
    getOriginAccessControlResponse_originAccessControl,
    getOriginAccessControlResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOriginAccessControl' smart constructor.
data GetOriginAccessControl = GetOriginAccessControl'
  { -- | The unique identifier of the origin access control.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginAccessControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getOriginAccessControl_id' - The unique identifier of the origin access control.
newGetOriginAccessControl ::
  -- | 'id'
  Prelude.Text ->
  GetOriginAccessControl
newGetOriginAccessControl pId_ =
  GetOriginAccessControl' {id = pId_}

-- | The unique identifier of the origin access control.
getOriginAccessControl_id :: Lens.Lens' GetOriginAccessControl Prelude.Text
getOriginAccessControl_id = Lens.lens (\GetOriginAccessControl' {id} -> id) (\s@GetOriginAccessControl' {} a -> s {id = a} :: GetOriginAccessControl)

instance Core.AWSRequest GetOriginAccessControl where
  type
    AWSResponse GetOriginAccessControl =
      GetOriginAccessControlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginAccessControlResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOriginAccessControl where
  hashWithSalt _salt GetOriginAccessControl' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetOriginAccessControl where
  rnf GetOriginAccessControl' {..} = Prelude.rnf id

instance Data.ToHeaders GetOriginAccessControl where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetOriginAccessControl where
  toPath GetOriginAccessControl' {..} =
    Prelude.mconcat
      ["/2020-05-31/origin-access-control/", Data.toBS id]

instance Data.ToQuery GetOriginAccessControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOriginAccessControlResponse' smart constructor.
data GetOriginAccessControlResponse = GetOriginAccessControlResponse'
  { -- | The version identifier for the current version of the origin access
    -- control.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Contains an origin access control, including its unique identifier.
    originAccessControl :: Prelude.Maybe OriginAccessControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginAccessControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getOriginAccessControlResponse_eTag' - The version identifier for the current version of the origin access
-- control.
--
-- 'originAccessControl', 'getOriginAccessControlResponse_originAccessControl' - Contains an origin access control, including its unique identifier.
--
-- 'httpStatus', 'getOriginAccessControlResponse_httpStatus' - The response's http status code.
newGetOriginAccessControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOriginAccessControlResponse
newGetOriginAccessControlResponse pHttpStatus_ =
  GetOriginAccessControlResponse'
    { eTag =
        Prelude.Nothing,
      originAccessControl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version identifier for the current version of the origin access
-- control.
getOriginAccessControlResponse_eTag :: Lens.Lens' GetOriginAccessControlResponse (Prelude.Maybe Prelude.Text)
getOriginAccessControlResponse_eTag = Lens.lens (\GetOriginAccessControlResponse' {eTag} -> eTag) (\s@GetOriginAccessControlResponse' {} a -> s {eTag = a} :: GetOriginAccessControlResponse)

-- | Contains an origin access control, including its unique identifier.
getOriginAccessControlResponse_originAccessControl :: Lens.Lens' GetOriginAccessControlResponse (Prelude.Maybe OriginAccessControl)
getOriginAccessControlResponse_originAccessControl = Lens.lens (\GetOriginAccessControlResponse' {originAccessControl} -> originAccessControl) (\s@GetOriginAccessControlResponse' {} a -> s {originAccessControl = a} :: GetOriginAccessControlResponse)

-- | The response's http status code.
getOriginAccessControlResponse_httpStatus :: Lens.Lens' GetOriginAccessControlResponse Prelude.Int
getOriginAccessControlResponse_httpStatus = Lens.lens (\GetOriginAccessControlResponse' {httpStatus} -> httpStatus) (\s@GetOriginAccessControlResponse' {} a -> s {httpStatus = a} :: GetOriginAccessControlResponse)

instance
  Prelude.NFData
    GetOriginAccessControlResponse
  where
  rnf GetOriginAccessControlResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf originAccessControl
      `Prelude.seq` Prelude.rnf httpStatus
