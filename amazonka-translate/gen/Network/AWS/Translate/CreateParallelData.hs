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
-- Module      : Network.AWS.Translate.CreateParallelData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a parallel data resource in Amazon Translate by importing an
-- input file from Amazon S3. Parallel data files contain examples of
-- source phrases and their translations from your translation memory. By
-- adding parallel data, you can influence the style, tone, and word choice
-- in your translation output.
module Network.AWS.Translate.CreateParallelData
  ( -- * Creating a Request
    CreateParallelData (..),
    newCreateParallelData,

    -- * Request Lenses
    createParallelData_encryptionKey,
    createParallelData_description,
    createParallelData_name,
    createParallelData_parallelDataConfig,
    createParallelData_clientToken,

    -- * Destructuring the Response
    CreateParallelDataResponse (..),
    newCreateParallelDataResponse,

    -- * Response Lenses
    createParallelDataResponse_status,
    createParallelDataResponse_name,
    createParallelDataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newCreateParallelData' smart constructor.
data CreateParallelData = CreateParallelData'
  { encryptionKey :: Core.Maybe EncryptionKey,
    -- | A custom description for the parallel data resource in Amazon Translate.
    description :: Core.Maybe Core.Text,
    -- | A custom name for the parallel data resource in Amazon Translate. You
    -- must assign a name that is unique in the account and region.
    name :: Core.Text,
    -- | Specifies the format and S3 location of the parallel data input file.
    parallelDataConfig :: ParallelDataConfig,
    -- | A unique identifier for the request. This token is automatically
    -- generated when you use Amazon Translate through an AWS SDK.
    clientToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateParallelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'createParallelData_encryptionKey' - Undocumented member.
--
-- 'description', 'createParallelData_description' - A custom description for the parallel data resource in Amazon Translate.
--
-- 'name', 'createParallelData_name' - A custom name for the parallel data resource in Amazon Translate. You
-- must assign a name that is unique in the account and region.
--
-- 'parallelDataConfig', 'createParallelData_parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
--
-- 'clientToken', 'createParallelData_clientToken' - A unique identifier for the request. This token is automatically
-- generated when you use Amazon Translate through an AWS SDK.
newCreateParallelData ::
  -- | 'name'
  Core.Text ->
  -- | 'parallelDataConfig'
  ParallelDataConfig ->
  -- | 'clientToken'
  Core.Text ->
  CreateParallelData
newCreateParallelData
  pName_
  pParallelDataConfig_
  pClientToken_ =
    CreateParallelData'
      { encryptionKey = Core.Nothing,
        description = Core.Nothing,
        name = pName_,
        parallelDataConfig = pParallelDataConfig_,
        clientToken = pClientToken_
      }

-- | Undocumented member.
createParallelData_encryptionKey :: Lens.Lens' CreateParallelData (Core.Maybe EncryptionKey)
createParallelData_encryptionKey = Lens.lens (\CreateParallelData' {encryptionKey} -> encryptionKey) (\s@CreateParallelData' {} a -> s {encryptionKey = a} :: CreateParallelData)

-- | A custom description for the parallel data resource in Amazon Translate.
createParallelData_description :: Lens.Lens' CreateParallelData (Core.Maybe Core.Text)
createParallelData_description = Lens.lens (\CreateParallelData' {description} -> description) (\s@CreateParallelData' {} a -> s {description = a} :: CreateParallelData)

-- | A custom name for the parallel data resource in Amazon Translate. You
-- must assign a name that is unique in the account and region.
createParallelData_name :: Lens.Lens' CreateParallelData Core.Text
createParallelData_name = Lens.lens (\CreateParallelData' {name} -> name) (\s@CreateParallelData' {} a -> s {name = a} :: CreateParallelData)

-- | Specifies the format and S3 location of the parallel data input file.
createParallelData_parallelDataConfig :: Lens.Lens' CreateParallelData ParallelDataConfig
createParallelData_parallelDataConfig = Lens.lens (\CreateParallelData' {parallelDataConfig} -> parallelDataConfig) (\s@CreateParallelData' {} a -> s {parallelDataConfig = a} :: CreateParallelData)

-- | A unique identifier for the request. This token is automatically
-- generated when you use Amazon Translate through an AWS SDK.
createParallelData_clientToken :: Lens.Lens' CreateParallelData Core.Text
createParallelData_clientToken = Lens.lens (\CreateParallelData' {clientToken} -> clientToken) (\s@CreateParallelData' {} a -> s {clientToken = a} :: CreateParallelData)

instance Core.AWSRequest CreateParallelData where
  type
    AWSResponse CreateParallelData =
      CreateParallelDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParallelDataResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateParallelData

instance Core.NFData CreateParallelData

instance Core.ToHeaders CreateParallelData where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.CreateParallelData" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateParallelData where
  toJSON CreateParallelData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncryptionKey" Core..=) Core.<$> encryptionKey,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("ParallelDataConfig" Core..= parallelDataConfig),
            Core.Just ("ClientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateParallelData where
  toPath = Core.const "/"

instance Core.ToQuery CreateParallelData where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateParallelDataResponse' smart constructor.
data CreateParallelDataResponse = CreateParallelDataResponse'
  { -- | The status of the parallel data resource. When the resource is ready for
    -- you to use, the status is @ACTIVE@.
    status :: Core.Maybe ParallelDataStatus,
    -- | The custom name that you assigned to the parallel data resource.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateParallelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createParallelDataResponse_status' - The status of the parallel data resource. When the resource is ready for
-- you to use, the status is @ACTIVE@.
--
-- 'name', 'createParallelDataResponse_name' - The custom name that you assigned to the parallel data resource.
--
-- 'httpStatus', 'createParallelDataResponse_httpStatus' - The response's http status code.
newCreateParallelDataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateParallelDataResponse
newCreateParallelDataResponse pHttpStatus_ =
  CreateParallelDataResponse'
    { status = Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the parallel data resource. When the resource is ready for
-- you to use, the status is @ACTIVE@.
createParallelDataResponse_status :: Lens.Lens' CreateParallelDataResponse (Core.Maybe ParallelDataStatus)
createParallelDataResponse_status = Lens.lens (\CreateParallelDataResponse' {status} -> status) (\s@CreateParallelDataResponse' {} a -> s {status = a} :: CreateParallelDataResponse)

-- | The custom name that you assigned to the parallel data resource.
createParallelDataResponse_name :: Lens.Lens' CreateParallelDataResponse (Core.Maybe Core.Text)
createParallelDataResponse_name = Lens.lens (\CreateParallelDataResponse' {name} -> name) (\s@CreateParallelDataResponse' {} a -> s {name = a} :: CreateParallelDataResponse)

-- | The response's http status code.
createParallelDataResponse_httpStatus :: Lens.Lens' CreateParallelDataResponse Core.Int
createParallelDataResponse_httpStatus = Lens.lens (\CreateParallelDataResponse' {httpStatus} -> httpStatus) (\s@CreateParallelDataResponse' {} a -> s {httpStatus = a} :: CreateParallelDataResponse)

instance Core.NFData CreateParallelDataResponse
