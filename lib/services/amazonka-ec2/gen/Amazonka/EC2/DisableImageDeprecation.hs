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
-- Module      : Amazonka.EC2.DisableImageDeprecation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the deprecation of the specified AMI.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-deprecate.html Deprecate an AMI>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DisableImageDeprecation
  ( -- * Creating a Request
    DisableImageDeprecation (..),
    newDisableImageDeprecation,

    -- * Request Lenses
    disableImageDeprecation_dryRun,
    disableImageDeprecation_imageId,

    -- * Destructuring the Response
    DisableImageDeprecationResponse (..),
    newDisableImageDeprecationResponse,

    -- * Response Lenses
    disableImageDeprecationResponse_return,
    disableImageDeprecationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableImageDeprecation' smart constructor.
data DisableImageDeprecation = DisableImageDeprecation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AMI.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableImageDeprecation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableImageDeprecation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'imageId', 'disableImageDeprecation_imageId' - The ID of the AMI.
newDisableImageDeprecation ::
  -- | 'imageId'
  Prelude.Text ->
  DisableImageDeprecation
newDisableImageDeprecation pImageId_ =
  DisableImageDeprecation'
    { dryRun = Prelude.Nothing,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableImageDeprecation_dryRun :: Lens.Lens' DisableImageDeprecation (Prelude.Maybe Prelude.Bool)
disableImageDeprecation_dryRun = Lens.lens (\DisableImageDeprecation' {dryRun} -> dryRun) (\s@DisableImageDeprecation' {} a -> s {dryRun = a} :: DisableImageDeprecation)

-- | The ID of the AMI.
disableImageDeprecation_imageId :: Lens.Lens' DisableImageDeprecation Prelude.Text
disableImageDeprecation_imageId = Lens.lens (\DisableImageDeprecation' {imageId} -> imageId) (\s@DisableImageDeprecation' {} a -> s {imageId = a} :: DisableImageDeprecation)

instance Core.AWSRequest DisableImageDeprecation where
  type
    AWSResponse DisableImageDeprecation =
      DisableImageDeprecationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableImageDeprecationResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableImageDeprecation where
  hashWithSalt _salt DisableImageDeprecation' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData DisableImageDeprecation where
  rnf DisableImageDeprecation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders DisableImageDeprecation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableImageDeprecation where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableImageDeprecation where
  toQuery DisableImageDeprecation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableImageDeprecation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newDisableImageDeprecationResponse' smart constructor.
data DisableImageDeprecationResponse = DisableImageDeprecationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableImageDeprecationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'disableImageDeprecationResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'disableImageDeprecationResponse_httpStatus' - The response's http status code.
newDisableImageDeprecationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableImageDeprecationResponse
newDisableImageDeprecationResponse pHttpStatus_ =
  DisableImageDeprecationResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disableImageDeprecationResponse_return :: Lens.Lens' DisableImageDeprecationResponse (Prelude.Maybe Prelude.Bool)
disableImageDeprecationResponse_return = Lens.lens (\DisableImageDeprecationResponse' {return'} -> return') (\s@DisableImageDeprecationResponse' {} a -> s {return' = a} :: DisableImageDeprecationResponse)

-- | The response's http status code.
disableImageDeprecationResponse_httpStatus :: Lens.Lens' DisableImageDeprecationResponse Prelude.Int
disableImageDeprecationResponse_httpStatus = Lens.lens (\DisableImageDeprecationResponse' {httpStatus} -> httpStatus) (\s@DisableImageDeprecationResponse' {} a -> s {httpStatus = a} :: DisableImageDeprecationResponse)

instance
  Prelude.NFData
    DisableImageDeprecationResponse
  where
  rnf DisableImageDeprecationResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
