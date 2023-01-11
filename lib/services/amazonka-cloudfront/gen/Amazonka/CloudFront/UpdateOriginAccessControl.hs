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
-- Module      : Amazonka.CloudFront.UpdateOriginAccessControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a CloudFront origin access control.
module Amazonka.CloudFront.UpdateOriginAccessControl
  ( -- * Creating a Request
    UpdateOriginAccessControl (..),
    newUpdateOriginAccessControl,

    -- * Request Lenses
    updateOriginAccessControl_ifMatch,
    updateOriginAccessControl_originAccessControlConfig,
    updateOriginAccessControl_id,

    -- * Destructuring the Response
    UpdateOriginAccessControlResponse (..),
    newUpdateOriginAccessControlResponse,

    -- * Response Lenses
    updateOriginAccessControlResponse_eTag,
    updateOriginAccessControlResponse_originAccessControl,
    updateOriginAccessControlResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOriginAccessControl' smart constructor.
data UpdateOriginAccessControl = UpdateOriginAccessControl'
  { -- | The current version (@ETag@ value) of the origin access control that you
    -- are updating.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | An origin access control.
    originAccessControlConfig :: OriginAccessControlConfig,
    -- | The unique identifier of the origin access control that you are
    -- updating.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOriginAccessControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateOriginAccessControl_ifMatch' - The current version (@ETag@ value) of the origin access control that you
-- are updating.
--
-- 'originAccessControlConfig', 'updateOriginAccessControl_originAccessControlConfig' - An origin access control.
--
-- 'id', 'updateOriginAccessControl_id' - The unique identifier of the origin access control that you are
-- updating.
newUpdateOriginAccessControl ::
  -- | 'originAccessControlConfig'
  OriginAccessControlConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateOriginAccessControl
newUpdateOriginAccessControl
  pOriginAccessControlConfig_
  pId_ =
    UpdateOriginAccessControl'
      { ifMatch =
          Prelude.Nothing,
        originAccessControlConfig =
          pOriginAccessControlConfig_,
        id = pId_
      }

-- | The current version (@ETag@ value) of the origin access control that you
-- are updating.
updateOriginAccessControl_ifMatch :: Lens.Lens' UpdateOriginAccessControl (Prelude.Maybe Prelude.Text)
updateOriginAccessControl_ifMatch = Lens.lens (\UpdateOriginAccessControl' {ifMatch} -> ifMatch) (\s@UpdateOriginAccessControl' {} a -> s {ifMatch = a} :: UpdateOriginAccessControl)

-- | An origin access control.
updateOriginAccessControl_originAccessControlConfig :: Lens.Lens' UpdateOriginAccessControl OriginAccessControlConfig
updateOriginAccessControl_originAccessControlConfig = Lens.lens (\UpdateOriginAccessControl' {originAccessControlConfig} -> originAccessControlConfig) (\s@UpdateOriginAccessControl' {} a -> s {originAccessControlConfig = a} :: UpdateOriginAccessControl)

-- | The unique identifier of the origin access control that you are
-- updating.
updateOriginAccessControl_id :: Lens.Lens' UpdateOriginAccessControl Prelude.Text
updateOriginAccessControl_id = Lens.lens (\UpdateOriginAccessControl' {id} -> id) (\s@UpdateOriginAccessControl' {} a -> s {id = a} :: UpdateOriginAccessControl)

instance Core.AWSRequest UpdateOriginAccessControl where
  type
    AWSResponse UpdateOriginAccessControl =
      UpdateOriginAccessControlResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateOriginAccessControlResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOriginAccessControl where
  hashWithSalt _salt UpdateOriginAccessControl' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` originAccessControlConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateOriginAccessControl where
  rnf UpdateOriginAccessControl' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf originAccessControlConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToElement UpdateOriginAccessControl where
  toElement UpdateOriginAccessControl' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}OriginAccessControlConfig"
      originAccessControlConfig

instance Data.ToHeaders UpdateOriginAccessControl where
  toHeaders UpdateOriginAccessControl' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateOriginAccessControl where
  toPath UpdateOriginAccessControl' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-access-control/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery UpdateOriginAccessControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOriginAccessControlResponse' smart constructor.
data UpdateOriginAccessControlResponse = UpdateOriginAccessControlResponse'
  { -- | The new version of the origin access control after it has been updated.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The origin access control after it has been updated.
    originAccessControl :: Prelude.Maybe OriginAccessControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOriginAccessControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateOriginAccessControlResponse_eTag' - The new version of the origin access control after it has been updated.
--
-- 'originAccessControl', 'updateOriginAccessControlResponse_originAccessControl' - The origin access control after it has been updated.
--
-- 'httpStatus', 'updateOriginAccessControlResponse_httpStatus' - The response's http status code.
newUpdateOriginAccessControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOriginAccessControlResponse
newUpdateOriginAccessControlResponse pHttpStatus_ =
  UpdateOriginAccessControlResponse'
    { eTag =
        Prelude.Nothing,
      originAccessControl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new version of the origin access control after it has been updated.
updateOriginAccessControlResponse_eTag :: Lens.Lens' UpdateOriginAccessControlResponse (Prelude.Maybe Prelude.Text)
updateOriginAccessControlResponse_eTag = Lens.lens (\UpdateOriginAccessControlResponse' {eTag} -> eTag) (\s@UpdateOriginAccessControlResponse' {} a -> s {eTag = a} :: UpdateOriginAccessControlResponse)

-- | The origin access control after it has been updated.
updateOriginAccessControlResponse_originAccessControl :: Lens.Lens' UpdateOriginAccessControlResponse (Prelude.Maybe OriginAccessControl)
updateOriginAccessControlResponse_originAccessControl = Lens.lens (\UpdateOriginAccessControlResponse' {originAccessControl} -> originAccessControl) (\s@UpdateOriginAccessControlResponse' {} a -> s {originAccessControl = a} :: UpdateOriginAccessControlResponse)

-- | The response's http status code.
updateOriginAccessControlResponse_httpStatus :: Lens.Lens' UpdateOriginAccessControlResponse Prelude.Int
updateOriginAccessControlResponse_httpStatus = Lens.lens (\UpdateOriginAccessControlResponse' {httpStatus} -> httpStatus) (\s@UpdateOriginAccessControlResponse' {} a -> s {httpStatus = a} :: UpdateOriginAccessControlResponse)

instance
  Prelude.NFData
    UpdateOriginAccessControlResponse
  where
  rnf UpdateOriginAccessControlResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf originAccessControl
      `Prelude.seq` Prelude.rnf httpStatus
