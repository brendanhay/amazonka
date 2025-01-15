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
-- Module      : Amazonka.CloudFront.CreateOriginAccessControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new origin access control in CloudFront. After you create an
-- origin access control, you can add it to an origin in a CloudFront
-- distribution so that CloudFront sends authenticated (signed) requests to
-- the origin.
--
-- For an Amazon S3 origin, this makes it possible to block public access
-- to the Amazon S3 bucket so that viewers (users) can access the content
-- in the bucket only through CloudFront.
--
-- For more information about using a CloudFront origin access control, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Restricting access to an Amazon S3 origin>
-- in the /Amazon CloudFront Developer Guide/.
module Amazonka.CloudFront.CreateOriginAccessControl
  ( -- * Creating a Request
    CreateOriginAccessControl (..),
    newCreateOriginAccessControl,

    -- * Request Lenses
    createOriginAccessControl_originAccessControlConfig,

    -- * Destructuring the Response
    CreateOriginAccessControlResponse (..),
    newCreateOriginAccessControlResponse,

    -- * Response Lenses
    createOriginAccessControlResponse_eTag,
    createOriginAccessControlResponse_location,
    createOriginAccessControlResponse_originAccessControl,
    createOriginAccessControlResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOriginAccessControl' smart constructor.
data CreateOriginAccessControl = CreateOriginAccessControl'
  { -- | Contains the origin access control.
    originAccessControlConfig :: OriginAccessControlConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOriginAccessControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccessControlConfig', 'createOriginAccessControl_originAccessControlConfig' - Contains the origin access control.
newCreateOriginAccessControl ::
  -- | 'originAccessControlConfig'
  OriginAccessControlConfig ->
  CreateOriginAccessControl
newCreateOriginAccessControl
  pOriginAccessControlConfig_ =
    CreateOriginAccessControl'
      { originAccessControlConfig =
          pOriginAccessControlConfig_
      }

-- | Contains the origin access control.
createOriginAccessControl_originAccessControlConfig :: Lens.Lens' CreateOriginAccessControl OriginAccessControlConfig
createOriginAccessControl_originAccessControlConfig = Lens.lens (\CreateOriginAccessControl' {originAccessControlConfig} -> originAccessControlConfig) (\s@CreateOriginAccessControl' {} a -> s {originAccessControlConfig = a} :: CreateOriginAccessControl)

instance Core.AWSRequest CreateOriginAccessControl where
  type
    AWSResponse CreateOriginAccessControl =
      CreateOriginAccessControlResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateOriginAccessControlResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOriginAccessControl where
  hashWithSalt _salt CreateOriginAccessControl' {..} =
    _salt
      `Prelude.hashWithSalt` originAccessControlConfig

instance Prelude.NFData CreateOriginAccessControl where
  rnf CreateOriginAccessControl' {..} =
    Prelude.rnf originAccessControlConfig

instance Data.ToElement CreateOriginAccessControl where
  toElement CreateOriginAccessControl' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}OriginAccessControlConfig"
      originAccessControlConfig

instance Data.ToHeaders CreateOriginAccessControl where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateOriginAccessControl where
  toPath =
    Prelude.const "/2020-05-31/origin-access-control"

instance Data.ToQuery CreateOriginAccessControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOriginAccessControlResponse' smart constructor.
data CreateOriginAccessControlResponse = CreateOriginAccessControlResponse'
  { -- | The version identifier for the current version of the origin access
    -- control.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The URL of the origin access control.
    location :: Prelude.Maybe Prelude.Text,
    -- | Contains an origin access control.
    originAccessControl :: Prelude.Maybe OriginAccessControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOriginAccessControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createOriginAccessControlResponse_eTag' - The version identifier for the current version of the origin access
-- control.
--
-- 'location', 'createOriginAccessControlResponse_location' - The URL of the origin access control.
--
-- 'originAccessControl', 'createOriginAccessControlResponse_originAccessControl' - Contains an origin access control.
--
-- 'httpStatus', 'createOriginAccessControlResponse_httpStatus' - The response's http status code.
newCreateOriginAccessControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOriginAccessControlResponse
newCreateOriginAccessControlResponse pHttpStatus_ =
  CreateOriginAccessControlResponse'
    { eTag =
        Prelude.Nothing,
      location = Prelude.Nothing,
      originAccessControl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version identifier for the current version of the origin access
-- control.
createOriginAccessControlResponse_eTag :: Lens.Lens' CreateOriginAccessControlResponse (Prelude.Maybe Prelude.Text)
createOriginAccessControlResponse_eTag = Lens.lens (\CreateOriginAccessControlResponse' {eTag} -> eTag) (\s@CreateOriginAccessControlResponse' {} a -> s {eTag = a} :: CreateOriginAccessControlResponse)

-- | The URL of the origin access control.
createOriginAccessControlResponse_location :: Lens.Lens' CreateOriginAccessControlResponse (Prelude.Maybe Prelude.Text)
createOriginAccessControlResponse_location = Lens.lens (\CreateOriginAccessControlResponse' {location} -> location) (\s@CreateOriginAccessControlResponse' {} a -> s {location = a} :: CreateOriginAccessControlResponse)

-- | Contains an origin access control.
createOriginAccessControlResponse_originAccessControl :: Lens.Lens' CreateOriginAccessControlResponse (Prelude.Maybe OriginAccessControl)
createOriginAccessControlResponse_originAccessControl = Lens.lens (\CreateOriginAccessControlResponse' {originAccessControl} -> originAccessControl) (\s@CreateOriginAccessControlResponse' {} a -> s {originAccessControl = a} :: CreateOriginAccessControlResponse)

-- | The response's http status code.
createOriginAccessControlResponse_httpStatus :: Lens.Lens' CreateOriginAccessControlResponse Prelude.Int
createOriginAccessControlResponse_httpStatus = Lens.lens (\CreateOriginAccessControlResponse' {httpStatus} -> httpStatus) (\s@CreateOriginAccessControlResponse' {} a -> s {httpStatus = a} :: CreateOriginAccessControlResponse)

instance
  Prelude.NFData
    CreateOriginAccessControlResponse
  where
  rnf CreateOriginAccessControlResponse' {..} =
    Prelude.rnf eTag `Prelude.seq`
      Prelude.rnf location `Prelude.seq`
        Prelude.rnf originAccessControl `Prelude.seq`
          Prelude.rnf httpStatus
