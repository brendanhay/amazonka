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
-- Module      : Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the provisioned concurrency configuration for a function.
module Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
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

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProvisionedConcurrencyConfig' smart constructor.
data DeleteProvisionedConcurrencyConfig = DeleteProvisionedConcurrencyConfig'
  { -- | The name of the Lambda function.
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
    functionName :: Core.Text,
    -- | The version number or alias name.
    qualifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
--
-- 'qualifier', 'deleteProvisionedConcurrencyConfig_qualifier' - The version number or alias name.
newDeleteProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Core.Text ->
  -- | 'qualifier'
  Core.Text ->
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
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
deleteProvisionedConcurrencyConfig_functionName :: Lens.Lens' DeleteProvisionedConcurrencyConfig Core.Text
deleteProvisionedConcurrencyConfig_functionName = Lens.lens (\DeleteProvisionedConcurrencyConfig' {functionName} -> functionName) (\s@DeleteProvisionedConcurrencyConfig' {} a -> s {functionName = a} :: DeleteProvisionedConcurrencyConfig)

-- | The version number or alias name.
deleteProvisionedConcurrencyConfig_qualifier :: Lens.Lens' DeleteProvisionedConcurrencyConfig Core.Text
deleteProvisionedConcurrencyConfig_qualifier = Lens.lens (\DeleteProvisionedConcurrencyConfig' {qualifier} -> qualifier) (\s@DeleteProvisionedConcurrencyConfig' {} a -> s {qualifier = a} :: DeleteProvisionedConcurrencyConfig)

instance
  Core.AWSRequest
    DeleteProvisionedConcurrencyConfig
  where
  type
    AWSResponse DeleteProvisionedConcurrencyConfig =
      DeleteProvisionedConcurrencyConfigResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteProvisionedConcurrencyConfigResponse'

instance
  Core.Hashable
    DeleteProvisionedConcurrencyConfig

instance
  Core.NFData
    DeleteProvisionedConcurrencyConfig

instance
  Core.ToHeaders
    DeleteProvisionedConcurrencyConfig
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteProvisionedConcurrencyConfig
  where
  toPath DeleteProvisionedConcurrencyConfig' {..} =
    Core.mconcat
      [ "/2019-09-30/functions/",
        Core.toBS functionName,
        "/provisioned-concurrency"
      ]

instance
  Core.ToQuery
    DeleteProvisionedConcurrencyConfig
  where
  toQuery DeleteProvisionedConcurrencyConfig' {..} =
    Core.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newDeleteProvisionedConcurrencyConfigResponse' smart constructor.
data DeleteProvisionedConcurrencyConfigResponse = DeleteProvisionedConcurrencyConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProvisionedConcurrencyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProvisionedConcurrencyConfigResponse ::
  DeleteProvisionedConcurrencyConfigResponse
newDeleteProvisionedConcurrencyConfigResponse =
  DeleteProvisionedConcurrencyConfigResponse'

instance
  Core.NFData
    DeleteProvisionedConcurrencyConfigResponse
