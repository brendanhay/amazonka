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
-- Module      : Network.AWS.CloudTrail.StopLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the recording of AWS API calls and log file delivery for the
-- specified trail. Under most circumstances, there is no need to use this
-- action. You can update a trail without stopping it first. This action is
-- the only way to stop recording. For a trail enabled in all regions, this
-- operation must be called from the region in which the trail was created,
-- or an @InvalidHomeRegionException@ will occur. This operation cannot be
-- called on the shadow trails (replicated trails in other regions) of a
-- trail enabled in all regions.
module Network.AWS.CloudTrail.StopLogging
  ( -- * Creating a Request
    StopLogging (..),
    newStopLogging,

    -- * Request Lenses
    stopLogging_name,

    -- * Destructuring the Response
    StopLoggingResponse (..),
    newStopLoggingResponse,

    -- * Response Lenses
    stopLoggingResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Passes the request to CloudTrail to stop logging AWS API calls for the
-- specified account.
--
-- /See:/ 'newStopLogging' smart constructor.
data StopLogging = StopLogging'
  { -- | Specifies the name or the CloudTrail ARN of the trail for which
    -- CloudTrail will stop logging AWS API calls. The format of a trail ARN
    -- is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopLogging_name' - Specifies the name or the CloudTrail ARN of the trail for which
-- CloudTrail will stop logging AWS API calls. The format of a trail ARN
-- is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newStopLogging ::
  -- | 'name'
  Prelude.Text ->
  StopLogging
newStopLogging pName_ = StopLogging' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which
-- CloudTrail will stop logging AWS API calls. The format of a trail ARN
-- is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
stopLogging_name :: Lens.Lens' StopLogging Prelude.Text
stopLogging_name = Lens.lens (\StopLogging' {name} -> name) (\s@StopLogging' {} a -> s {name = a} :: StopLogging)

instance Prelude.AWSRequest StopLogging where
  type Rs StopLogging = StopLoggingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopLoggingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopLogging

instance Prelude.NFData StopLogging

instance Prelude.ToHeaders StopLogging where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopLogging" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopLogging where
  toJSON StopLogging' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StopLogging where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopLogging where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newStopLoggingResponse' smart constructor.
data StopLoggingResponse = StopLoggingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopLoggingResponse_httpStatus' - The response's http status code.
newStopLoggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopLoggingResponse
newStopLoggingResponse pHttpStatus_ =
  StopLoggingResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopLoggingResponse_httpStatus :: Lens.Lens' StopLoggingResponse Prelude.Int
stopLoggingResponse_httpStatus = Lens.lens (\StopLoggingResponse' {httpStatus} -> httpStatus) (\s@StopLoggingResponse' {} a -> s {httpStatus = a} :: StopLoggingResponse)

instance Prelude.NFData StopLoggingResponse
