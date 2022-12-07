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
-- Module      : Amazonka.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the recording of Amazon Web Services API calls and log file
-- delivery for a trail. For a trail that is enabled in all regions, this
-- operation must be called from the region in which the trail was created.
-- This operation cannot be called on the shadow trails (replicated trails
-- in other regions) of a trail that is enabled in all regions.
module Amazonka.CloudTrail.StartLogging
  ( -- * Creating a Request
    StartLogging (..),
    newStartLogging,

    -- * Request Lenses
    startLogging_name,

    -- * Destructuring the Response
    StartLoggingResponse (..),
    newStartLoggingResponse,

    -- * Response Lenses
    startLoggingResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to CloudTrail to start logging Amazon Web Services API calls
-- for an account.
--
-- /See:/ 'newStartLogging' smart constructor.
data StartLogging = StartLogging'
  { -- | Specifies the name or the CloudTrail ARN of the trail for which
    -- CloudTrail logs Amazon Web Services API calls. The following is the
    -- format of a trail ARN.
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startLogging_name' - Specifies the name or the CloudTrail ARN of the trail for which
-- CloudTrail logs Amazon Web Services API calls. The following is the
-- format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newStartLogging ::
  -- | 'name'
  Prelude.Text ->
  StartLogging
newStartLogging pName_ = StartLogging' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which
-- CloudTrail logs Amazon Web Services API calls. The following is the
-- format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
startLogging_name :: Lens.Lens' StartLogging Prelude.Text
startLogging_name = Lens.lens (\StartLogging' {name} -> name) (\s@StartLogging' {} a -> s {name = a} :: StartLogging)

instance Core.AWSRequest StartLogging where
  type AWSResponse StartLogging = StartLoggingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartLoggingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartLogging where
  hashWithSalt _salt StartLogging' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StartLogging where
  rnf StartLogging' {..} = Prelude.rnf name

instance Data.ToHeaders StartLogging where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StartLogging" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartLogging where
  toJSON StartLogging' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath StartLogging where
  toPath = Prelude.const "/"

instance Data.ToQuery StartLogging where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newStartLoggingResponse' smart constructor.
data StartLoggingResponse = StartLoggingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startLoggingResponse_httpStatus' - The response's http status code.
newStartLoggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartLoggingResponse
newStartLoggingResponse pHttpStatus_ =
  StartLoggingResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startLoggingResponse_httpStatus :: Lens.Lens' StartLoggingResponse Prelude.Int
startLoggingResponse_httpStatus = Lens.lens (\StartLoggingResponse' {httpStatus} -> httpStatus) (\s@StartLoggingResponse' {} a -> s {httpStatus = a} :: StartLoggingResponse)

instance Prelude.NFData StartLoggingResponse where
  rnf StartLoggingResponse' {..} =
    Prelude.rnf httpStatus
