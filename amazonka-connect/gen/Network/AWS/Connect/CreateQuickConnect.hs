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
-- Module      : Network.AWS.Connect.CreateQuickConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Creates a quick connect for the specified Amazon Connect instance.
module Network.AWS.Connect.CreateQuickConnect
  ( -- * Creating a Request
    CreateQuickConnect (..),
    newCreateQuickConnect,

    -- * Request Lenses
    createQuickConnect_tags,
    createQuickConnect_description,
    createQuickConnect_instanceId,
    createQuickConnect_name,
    createQuickConnect_quickConnectConfig,

    -- * Destructuring the Response
    CreateQuickConnectResponse (..),
    newCreateQuickConnectResponse,

    -- * Response Lenses
    createQuickConnectResponse_quickConnectId,
    createQuickConnectResponse_quickConnectARN,
    createQuickConnectResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateQuickConnect' smart constructor.
data CreateQuickConnect = CreateQuickConnect'
  { -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the quick connect.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The name of the quick connect.
    name :: Prelude.Text,
    -- | Configuration settings for the quick connect.
    quickConnectConfig :: QuickConnectConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQuickConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createQuickConnect_tags' - One or more tags.
--
-- 'description', 'createQuickConnect_description' - The description of the quick connect.
--
-- 'instanceId', 'createQuickConnect_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'name', 'createQuickConnect_name' - The name of the quick connect.
--
-- 'quickConnectConfig', 'createQuickConnect_quickConnectConfig' - Configuration settings for the quick connect.
newCreateQuickConnect ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'quickConnectConfig'
  QuickConnectConfig ->
  CreateQuickConnect
newCreateQuickConnect
  pInstanceId_
  pName_
  pQuickConnectConfig_ =
    CreateQuickConnect'
      { tags = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        quickConnectConfig = pQuickConnectConfig_
      }

-- | One or more tags.
createQuickConnect_tags :: Lens.Lens' CreateQuickConnect (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createQuickConnect_tags = Lens.lens (\CreateQuickConnect' {tags} -> tags) (\s@CreateQuickConnect' {} a -> s {tags = a} :: CreateQuickConnect) Prelude.. Lens.mapping Lens._Coerce

-- | The description of the quick connect.
createQuickConnect_description :: Lens.Lens' CreateQuickConnect (Prelude.Maybe Prelude.Text)
createQuickConnect_description = Lens.lens (\CreateQuickConnect' {description} -> description) (\s@CreateQuickConnect' {} a -> s {description = a} :: CreateQuickConnect)

-- | The identifier of the Amazon Connect instance.
createQuickConnect_instanceId :: Lens.Lens' CreateQuickConnect Prelude.Text
createQuickConnect_instanceId = Lens.lens (\CreateQuickConnect' {instanceId} -> instanceId) (\s@CreateQuickConnect' {} a -> s {instanceId = a} :: CreateQuickConnect)

-- | The name of the quick connect.
createQuickConnect_name :: Lens.Lens' CreateQuickConnect Prelude.Text
createQuickConnect_name = Lens.lens (\CreateQuickConnect' {name} -> name) (\s@CreateQuickConnect' {} a -> s {name = a} :: CreateQuickConnect)

-- | Configuration settings for the quick connect.
createQuickConnect_quickConnectConfig :: Lens.Lens' CreateQuickConnect QuickConnectConfig
createQuickConnect_quickConnectConfig = Lens.lens (\CreateQuickConnect' {quickConnectConfig} -> quickConnectConfig) (\s@CreateQuickConnect' {} a -> s {quickConnectConfig = a} :: CreateQuickConnect)

instance Core.AWSRequest CreateQuickConnect where
  type
    AWSResponse CreateQuickConnect =
      CreateQuickConnectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQuickConnectResponse'
            Prelude.<$> (x Core..?> "QuickConnectId")
            Prelude.<*> (x Core..?> "QuickConnectARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateQuickConnect

instance Prelude.NFData CreateQuickConnect

instance Core.ToHeaders CreateQuickConnect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateQuickConnect where
  toJSON CreateQuickConnect' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("QuickConnectConfig" Core..= quickConnectConfig)
          ]
      )

instance Core.ToPath CreateQuickConnect where
  toPath CreateQuickConnect' {..} =
    Prelude.mconcat
      ["/quick-connects/", Core.toBS instanceId]

instance Core.ToQuery CreateQuickConnect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateQuickConnectResponse' smart constructor.
data CreateQuickConnectResponse = CreateQuickConnectResponse'
  { -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the quick connect.
    quickConnectARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQuickConnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quickConnectId', 'createQuickConnectResponse_quickConnectId' - The identifier for the quick connect.
--
-- 'quickConnectARN', 'createQuickConnectResponse_quickConnectARN' - The Amazon Resource Name (ARN) for the quick connect.
--
-- 'httpStatus', 'createQuickConnectResponse_httpStatus' - The response's http status code.
newCreateQuickConnectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateQuickConnectResponse
newCreateQuickConnectResponse pHttpStatus_ =
  CreateQuickConnectResponse'
    { quickConnectId =
        Prelude.Nothing,
      quickConnectARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the quick connect.
createQuickConnectResponse_quickConnectId :: Lens.Lens' CreateQuickConnectResponse (Prelude.Maybe Prelude.Text)
createQuickConnectResponse_quickConnectId = Lens.lens (\CreateQuickConnectResponse' {quickConnectId} -> quickConnectId) (\s@CreateQuickConnectResponse' {} a -> s {quickConnectId = a} :: CreateQuickConnectResponse)

-- | The Amazon Resource Name (ARN) for the quick connect.
createQuickConnectResponse_quickConnectARN :: Lens.Lens' CreateQuickConnectResponse (Prelude.Maybe Prelude.Text)
createQuickConnectResponse_quickConnectARN = Lens.lens (\CreateQuickConnectResponse' {quickConnectARN} -> quickConnectARN) (\s@CreateQuickConnectResponse' {} a -> s {quickConnectARN = a} :: CreateQuickConnectResponse)

-- | The response's http status code.
createQuickConnectResponse_httpStatus :: Lens.Lens' CreateQuickConnectResponse Prelude.Int
createQuickConnectResponse_httpStatus = Lens.lens (\CreateQuickConnectResponse' {httpStatus} -> httpStatus) (\s@CreateQuickConnectResponse' {} a -> s {httpStatus = a} :: CreateQuickConnectResponse)

instance Prelude.NFData CreateQuickConnectResponse
