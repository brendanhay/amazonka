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
-- Module      : Amazonka.ChimeSDKIdentity.DescribeAppInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of an @AppInstance@.
module Amazonka.ChimeSDKIdentity.DescribeAppInstance
  ( -- * Creating a Request
    DescribeAppInstance (..),
    newDescribeAppInstance,

    -- * Request Lenses
    describeAppInstance_appInstanceArn,

    -- * Destructuring the Response
    DescribeAppInstanceResponse (..),
    newDescribeAppInstanceResponse,

    -- * Response Lenses
    describeAppInstanceResponse_appInstance,
    describeAppInstanceResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppInstance' smart constructor.
data DescribeAppInstance = DescribeAppInstance'
  { -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'describeAppInstance_appInstanceArn' - The ARN of the @AppInstance@.
newDescribeAppInstance ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  DescribeAppInstance
newDescribeAppInstance pAppInstanceArn_ =
  DescribeAppInstance'
    { appInstanceArn =
        pAppInstanceArn_
    }

-- | The ARN of the @AppInstance@.
describeAppInstance_appInstanceArn :: Lens.Lens' DescribeAppInstance Prelude.Text
describeAppInstance_appInstanceArn = Lens.lens (\DescribeAppInstance' {appInstanceArn} -> appInstanceArn) (\s@DescribeAppInstance' {} a -> s {appInstanceArn = a} :: DescribeAppInstance)

instance Core.AWSRequest DescribeAppInstance where
  type
    AWSResponse DescribeAppInstance =
      DescribeAppInstanceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppInstanceResponse'
            Prelude.<$> (x Data..?> "AppInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAppInstance where
  hashWithSalt _salt DescribeAppInstance' {..} =
    _salt `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData DescribeAppInstance where
  rnf DescribeAppInstance' {..} =
    Prelude.rnf appInstanceArn

instance Data.ToHeaders DescribeAppInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAppInstance where
  toPath DescribeAppInstance' {..} =
    Prelude.mconcat
      ["/app-instances/", Data.toBS appInstanceArn]

instance Data.ToQuery DescribeAppInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppInstanceResponse' smart constructor.
data DescribeAppInstanceResponse = DescribeAppInstanceResponse'
  { -- | The ARN, metadata, created and last-updated timestamps, and the name of
    -- the @AppInstance@. All timestamps use epoch milliseconds.
    appInstance :: Prelude.Maybe AppInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstance', 'describeAppInstanceResponse_appInstance' - The ARN, metadata, created and last-updated timestamps, and the name of
-- the @AppInstance@. All timestamps use epoch milliseconds.
--
-- 'httpStatus', 'describeAppInstanceResponse_httpStatus' - The response's http status code.
newDescribeAppInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppInstanceResponse
newDescribeAppInstanceResponse pHttpStatus_ =
  DescribeAppInstanceResponse'
    { appInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN, metadata, created and last-updated timestamps, and the name of
-- the @AppInstance@. All timestamps use epoch milliseconds.
describeAppInstanceResponse_appInstance :: Lens.Lens' DescribeAppInstanceResponse (Prelude.Maybe AppInstance)
describeAppInstanceResponse_appInstance = Lens.lens (\DescribeAppInstanceResponse' {appInstance} -> appInstance) (\s@DescribeAppInstanceResponse' {} a -> s {appInstance = a} :: DescribeAppInstanceResponse)

-- | The response's http status code.
describeAppInstanceResponse_httpStatus :: Lens.Lens' DescribeAppInstanceResponse Prelude.Int
describeAppInstanceResponse_httpStatus = Lens.lens (\DescribeAppInstanceResponse' {httpStatus} -> httpStatus) (\s@DescribeAppInstanceResponse' {} a -> s {httpStatus = a} :: DescribeAppInstanceResponse)

instance Prelude.NFData DescribeAppInstanceResponse where
  rnf DescribeAppInstanceResponse' {..} =
    Prelude.rnf appInstance
      `Prelude.seq` Prelude.rnf httpStatus
