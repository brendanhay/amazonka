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
-- Module      : Amazonka.ChimeSDKIdentity.DescribeAppInstanceBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AppInstanceBot\'s@ information.
module Amazonka.ChimeSDKIdentity.DescribeAppInstanceBot
  ( -- * Creating a Request
    DescribeAppInstanceBot (..),
    newDescribeAppInstanceBot,

    -- * Request Lenses
    describeAppInstanceBot_appInstanceBotArn,

    -- * Destructuring the Response
    DescribeAppInstanceBotResponse (..),
    newDescribeAppInstanceBotResponse,

    -- * Response Lenses
    describeAppInstanceBotResponse_appInstanceBot,
    describeAppInstanceBotResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppInstanceBot' smart constructor.
data DescribeAppInstanceBot = DescribeAppInstanceBot'
  { -- | The ARN of the @AppInstanceBot@.
    appInstanceBotArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBotArn', 'describeAppInstanceBot_appInstanceBotArn' - The ARN of the @AppInstanceBot@.
newDescribeAppInstanceBot ::
  -- | 'appInstanceBotArn'
  Prelude.Text ->
  DescribeAppInstanceBot
newDescribeAppInstanceBot pAppInstanceBotArn_ =
  DescribeAppInstanceBot'
    { appInstanceBotArn =
        pAppInstanceBotArn_
    }

-- | The ARN of the @AppInstanceBot@.
describeAppInstanceBot_appInstanceBotArn :: Lens.Lens' DescribeAppInstanceBot Prelude.Text
describeAppInstanceBot_appInstanceBotArn = Lens.lens (\DescribeAppInstanceBot' {appInstanceBotArn} -> appInstanceBotArn) (\s@DescribeAppInstanceBot' {} a -> s {appInstanceBotArn = a} :: DescribeAppInstanceBot)

instance Core.AWSRequest DescribeAppInstanceBot where
  type
    AWSResponse DescribeAppInstanceBot =
      DescribeAppInstanceBotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppInstanceBotResponse'
            Prelude.<$> (x Data..?> "AppInstanceBot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAppInstanceBot where
  hashWithSalt _salt DescribeAppInstanceBot' {..} =
    _salt `Prelude.hashWithSalt` appInstanceBotArn

instance Prelude.NFData DescribeAppInstanceBot where
  rnf DescribeAppInstanceBot' {..} =
    Prelude.rnf appInstanceBotArn

instance Data.ToHeaders DescribeAppInstanceBot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAppInstanceBot where
  toPath DescribeAppInstanceBot' {..} =
    Prelude.mconcat
      ["/app-instance-bots/", Data.toBS appInstanceBotArn]

instance Data.ToQuery DescribeAppInstanceBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppInstanceBotResponse' smart constructor.
data DescribeAppInstanceBotResponse = DescribeAppInstanceBotResponse'
  { -- | The detials of the @AppInstanceBot@.
    appInstanceBot :: Prelude.Maybe AppInstanceBot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBot', 'describeAppInstanceBotResponse_appInstanceBot' - The detials of the @AppInstanceBot@.
--
-- 'httpStatus', 'describeAppInstanceBotResponse_httpStatus' - The response's http status code.
newDescribeAppInstanceBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppInstanceBotResponse
newDescribeAppInstanceBotResponse pHttpStatus_ =
  DescribeAppInstanceBotResponse'
    { appInstanceBot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detials of the @AppInstanceBot@.
describeAppInstanceBotResponse_appInstanceBot :: Lens.Lens' DescribeAppInstanceBotResponse (Prelude.Maybe AppInstanceBot)
describeAppInstanceBotResponse_appInstanceBot = Lens.lens (\DescribeAppInstanceBotResponse' {appInstanceBot} -> appInstanceBot) (\s@DescribeAppInstanceBotResponse' {} a -> s {appInstanceBot = a} :: DescribeAppInstanceBotResponse)

-- | The response's http status code.
describeAppInstanceBotResponse_httpStatus :: Lens.Lens' DescribeAppInstanceBotResponse Prelude.Int
describeAppInstanceBotResponse_httpStatus = Lens.lens (\DescribeAppInstanceBotResponse' {httpStatus} -> httpStatus) (\s@DescribeAppInstanceBotResponse' {} a -> s {httpStatus = a} :: DescribeAppInstanceBotResponse)

instance
  Prelude.NFData
    DescribeAppInstanceBotResponse
  where
  rnf DescribeAppInstanceBotResponse' {..} =
    Prelude.rnf appInstanceBot
      `Prelude.seq` Prelude.rnf httpStatus
