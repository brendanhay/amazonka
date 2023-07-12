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
-- Module      : Amazonka.EC2.EnableImageDeprecation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables deprecation of the specified AMI at the specified date and time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-deprecate.html Deprecate an AMI>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.EnableImageDeprecation
  ( -- * Creating a Request
    EnableImageDeprecation (..),
    newEnableImageDeprecation,

    -- * Request Lenses
    enableImageDeprecation_dryRun,
    enableImageDeprecation_imageId,
    enableImageDeprecation_deprecateAt,

    -- * Destructuring the Response
    EnableImageDeprecationResponse (..),
    newEnableImageDeprecationResponse,

    -- * Response Lenses
    enableImageDeprecationResponse_return,
    enableImageDeprecationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableImageDeprecation' smart constructor.
data EnableImageDeprecation = EnableImageDeprecation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AMI.
    imageId :: Prelude.Text,
    -- | The date and time to deprecate the AMI, in UTC, in the following format:
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specify a value for seconds,
    -- Amazon EC2 rounds the seconds to the nearest minute.
    --
    -- You can’t specify a date in the past. The upper limit for @DeprecateAt@
    -- is 10 years from now, except for public AMIs, where the upper limit is 2
    -- years from the creation date.
    deprecateAt :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableImageDeprecation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableImageDeprecation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'imageId', 'enableImageDeprecation_imageId' - The ID of the AMI.
--
-- 'deprecateAt', 'enableImageDeprecation_deprecateAt' - The date and time to deprecate the AMI, in UTC, in the following format:
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specify a value for seconds,
-- Amazon EC2 rounds the seconds to the nearest minute.
--
-- You can’t specify a date in the past. The upper limit for @DeprecateAt@
-- is 10 years from now, except for public AMIs, where the upper limit is 2
-- years from the creation date.
newEnableImageDeprecation ::
  -- | 'imageId'
  Prelude.Text ->
  -- | 'deprecateAt'
  Prelude.UTCTime ->
  EnableImageDeprecation
newEnableImageDeprecation pImageId_ pDeprecateAt_ =
  EnableImageDeprecation'
    { dryRun = Prelude.Nothing,
      imageId = pImageId_,
      deprecateAt = Data._Time Lens.# pDeprecateAt_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableImageDeprecation_dryRun :: Lens.Lens' EnableImageDeprecation (Prelude.Maybe Prelude.Bool)
enableImageDeprecation_dryRun = Lens.lens (\EnableImageDeprecation' {dryRun} -> dryRun) (\s@EnableImageDeprecation' {} a -> s {dryRun = a} :: EnableImageDeprecation)

-- | The ID of the AMI.
enableImageDeprecation_imageId :: Lens.Lens' EnableImageDeprecation Prelude.Text
enableImageDeprecation_imageId = Lens.lens (\EnableImageDeprecation' {imageId} -> imageId) (\s@EnableImageDeprecation' {} a -> s {imageId = a} :: EnableImageDeprecation)

-- | The date and time to deprecate the AMI, in UTC, in the following format:
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specify a value for seconds,
-- Amazon EC2 rounds the seconds to the nearest minute.
--
-- You can’t specify a date in the past. The upper limit for @DeprecateAt@
-- is 10 years from now, except for public AMIs, where the upper limit is 2
-- years from the creation date.
enableImageDeprecation_deprecateAt :: Lens.Lens' EnableImageDeprecation Prelude.UTCTime
enableImageDeprecation_deprecateAt = Lens.lens (\EnableImageDeprecation' {deprecateAt} -> deprecateAt) (\s@EnableImageDeprecation' {} a -> s {deprecateAt = a} :: EnableImageDeprecation) Prelude.. Data._Time

instance Core.AWSRequest EnableImageDeprecation where
  type
    AWSResponse EnableImageDeprecation =
      EnableImageDeprecationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableImageDeprecationResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableImageDeprecation where
  hashWithSalt _salt EnableImageDeprecation' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` deprecateAt

instance Prelude.NFData EnableImageDeprecation where
  rnf EnableImageDeprecation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf deprecateAt

instance Data.ToHeaders EnableImageDeprecation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableImageDeprecation where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableImageDeprecation where
  toQuery EnableImageDeprecation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableImageDeprecation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ImageId" Data.=: imageId,
        "DeprecateAt" Data.=: deprecateAt
      ]

-- | /See:/ 'newEnableImageDeprecationResponse' smart constructor.
data EnableImageDeprecationResponse = EnableImageDeprecationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableImageDeprecationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'enableImageDeprecationResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'enableImageDeprecationResponse_httpStatus' - The response's http status code.
newEnableImageDeprecationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableImageDeprecationResponse
newEnableImageDeprecationResponse pHttpStatus_ =
  EnableImageDeprecationResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
enableImageDeprecationResponse_return :: Lens.Lens' EnableImageDeprecationResponse (Prelude.Maybe Prelude.Bool)
enableImageDeprecationResponse_return = Lens.lens (\EnableImageDeprecationResponse' {return'} -> return') (\s@EnableImageDeprecationResponse' {} a -> s {return' = a} :: EnableImageDeprecationResponse)

-- | The response's http status code.
enableImageDeprecationResponse_httpStatus :: Lens.Lens' EnableImageDeprecationResponse Prelude.Int
enableImageDeprecationResponse_httpStatus = Lens.lens (\EnableImageDeprecationResponse' {httpStatus} -> httpStatus) (\s@EnableImageDeprecationResponse' {} a -> s {httpStatus = a} :: EnableImageDeprecationResponse)

instance
  Prelude.NFData
    EnableImageDeprecationResponse
  where
  rnf EnableImageDeprecationResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
