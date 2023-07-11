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
-- Module      : Amazonka.Lambda.DeleteProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the provisioned concurrency configuration for a function.
module Amazonka.Lambda.DeleteProvisionedConcurrencyConfig
  ( -- * Creating a Request
    DeleteProvisionedConcurrencyConfig (..),
    newDeleteProvisionedConcurrencyConfig,

    -- * Request Lenses
    deleteProvisionedConcurrencyConfig_functionName,
    deleteProvisionedConcurrencyConfig_qualifier,

    -- * Destructuring the Response
    DeleteProvisionedConcurrencyConfigResponse (..),
    newDeleteProvisionedConcurrencyConfigResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProvisionedConcurrencyConfig' smart constructor.
data DeleteProvisionedConcurrencyConfig = DeleteProvisionedConcurrencyConfig'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text,
    -- | The version number or alias name.
    qualifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisionedConcurrencyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'deleteProvisionedConcurrencyConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
--
-- 'qualifier', 'deleteProvisionedConcurrencyConfig_qualifier' - The version number or alias name.
newDeleteProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'qualifier'
  Prelude.Text ->
  DeleteProvisionedConcurrencyConfig
newDeleteProvisionedConcurrencyConfig
  pFunctionName_
  pQualifier_ =
    DeleteProvisionedConcurrencyConfig'
      { functionName =
          pFunctionName_,
        qualifier = pQualifier_
      }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
deleteProvisionedConcurrencyConfig_functionName :: Lens.Lens' DeleteProvisionedConcurrencyConfig Prelude.Text
deleteProvisionedConcurrencyConfig_functionName = Lens.lens (\DeleteProvisionedConcurrencyConfig' {functionName} -> functionName) (\s@DeleteProvisionedConcurrencyConfig' {} a -> s {functionName = a} :: DeleteProvisionedConcurrencyConfig)

-- | The version number or alias name.
deleteProvisionedConcurrencyConfig_qualifier :: Lens.Lens' DeleteProvisionedConcurrencyConfig Prelude.Text
deleteProvisionedConcurrencyConfig_qualifier = Lens.lens (\DeleteProvisionedConcurrencyConfig' {qualifier} -> qualifier) (\s@DeleteProvisionedConcurrencyConfig' {} a -> s {qualifier = a} :: DeleteProvisionedConcurrencyConfig)

instance
  Core.AWSRequest
    DeleteProvisionedConcurrencyConfig
  where
  type
    AWSResponse DeleteProvisionedConcurrencyConfig =
      DeleteProvisionedConcurrencyConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteProvisionedConcurrencyConfigResponse'

instance
  Prelude.Hashable
    DeleteProvisionedConcurrencyConfig
  where
  hashWithSalt
    _salt
    DeleteProvisionedConcurrencyConfig' {..} =
      _salt
        `Prelude.hashWithSalt` functionName
        `Prelude.hashWithSalt` qualifier

instance
  Prelude.NFData
    DeleteProvisionedConcurrencyConfig
  where
  rnf DeleteProvisionedConcurrencyConfig' {..} =
    Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf qualifier

instance
  Data.ToHeaders
    DeleteProvisionedConcurrencyConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteProvisionedConcurrencyConfig
  where
  toPath DeleteProvisionedConcurrencyConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-30/functions/",
        Data.toBS functionName,
        "/provisioned-concurrency"
      ]

instance
  Data.ToQuery
    DeleteProvisionedConcurrencyConfig
  where
  toQuery DeleteProvisionedConcurrencyConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newDeleteProvisionedConcurrencyConfigResponse' smart constructor.
data DeleteProvisionedConcurrencyConfigResponse = DeleteProvisionedConcurrencyConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisionedConcurrencyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProvisionedConcurrencyConfigResponse ::
  DeleteProvisionedConcurrencyConfigResponse
newDeleteProvisionedConcurrencyConfigResponse =
  DeleteProvisionedConcurrencyConfigResponse'

instance
  Prelude.NFData
    DeleteProvisionedConcurrencyConfigResponse
  where
  rnf _ = ()
