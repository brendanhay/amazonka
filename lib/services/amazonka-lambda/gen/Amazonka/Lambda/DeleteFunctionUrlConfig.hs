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
-- Module      : Amazonka.Lambda.DeleteFunctionUrlConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function URL. When you delete a function URL, you
-- can\'t recover it. Creating a new function URL results in a different
-- URL address.
module Amazonka.Lambda.DeleteFunctionUrlConfig
  ( -- * Creating a Request
    DeleteFunctionUrlConfig (..),
    newDeleteFunctionUrlConfig,

    -- * Request Lenses
    deleteFunctionUrlConfig_qualifier,
    deleteFunctionUrlConfig_functionName,

    -- * Destructuring the Response
    DeleteFunctionUrlConfigResponse (..),
    newDeleteFunctionUrlConfigResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFunctionUrlConfig' smart constructor.
data DeleteFunctionUrlConfig = DeleteFunctionUrlConfig'
  { -- | The alias name.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'deleteFunctionUrlConfig_qualifier' - The alias name.
--
-- 'functionName', 'deleteFunctionUrlConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newDeleteFunctionUrlConfig ::
  -- | 'functionName'
  Prelude.Text ->
  DeleteFunctionUrlConfig
newDeleteFunctionUrlConfig pFunctionName_ =
  DeleteFunctionUrlConfig'
    { qualifier =
        Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The alias name.
deleteFunctionUrlConfig_qualifier :: Lens.Lens' DeleteFunctionUrlConfig (Prelude.Maybe Prelude.Text)
deleteFunctionUrlConfig_qualifier = Lens.lens (\DeleteFunctionUrlConfig' {qualifier} -> qualifier) (\s@DeleteFunctionUrlConfig' {} a -> s {qualifier = a} :: DeleteFunctionUrlConfig)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
deleteFunctionUrlConfig_functionName :: Lens.Lens' DeleteFunctionUrlConfig Prelude.Text
deleteFunctionUrlConfig_functionName = Lens.lens (\DeleteFunctionUrlConfig' {functionName} -> functionName) (\s@DeleteFunctionUrlConfig' {} a -> s {functionName = a} :: DeleteFunctionUrlConfig)

instance Core.AWSRequest DeleteFunctionUrlConfig where
  type
    AWSResponse DeleteFunctionUrlConfig =
      DeleteFunctionUrlConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteFunctionUrlConfigResponse'

instance Prelude.Hashable DeleteFunctionUrlConfig where
  hashWithSalt _salt DeleteFunctionUrlConfig' {..} =
    _salt `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData DeleteFunctionUrlConfig where
  rnf DeleteFunctionUrlConfig' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Core.ToHeaders DeleteFunctionUrlConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteFunctionUrlConfig where
  toPath DeleteFunctionUrlConfig' {..} =
    Prelude.mconcat
      [ "/2021-10-31/functions/",
        Core.toBS functionName,
        "/url"
      ]

instance Core.ToQuery DeleteFunctionUrlConfig where
  toQuery DeleteFunctionUrlConfig' {..} =
    Prelude.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newDeleteFunctionUrlConfigResponse' smart constructor.
data DeleteFunctionUrlConfigResponse = DeleteFunctionUrlConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionUrlConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFunctionUrlConfigResponse ::
  DeleteFunctionUrlConfigResponse
newDeleteFunctionUrlConfigResponse =
  DeleteFunctionUrlConfigResponse'

instance
  Prelude.NFData
    DeleteFunctionUrlConfigResponse
  where
  rnf _ = ()
