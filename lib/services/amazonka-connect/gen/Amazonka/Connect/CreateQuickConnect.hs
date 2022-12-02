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
-- Module      : Amazonka.Connect.CreateQuickConnect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a quick connect for the specified Amazon Connect instance.
module Amazonka.Connect.CreateQuickConnect
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
    createQuickConnectResponse_quickConnectARN,
    createQuickConnectResponse_quickConnectId,
    createQuickConnectResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateQuickConnect' smart constructor.
data CreateQuickConnect = CreateQuickConnect'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the quick connect.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
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
-- 'tags', 'createQuickConnect_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'description', 'createQuickConnect_description' - The description of the quick connect.
--
-- 'instanceId', 'createQuickConnect_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createQuickConnect_tags :: Lens.Lens' CreateQuickConnect (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createQuickConnect_tags = Lens.lens (\CreateQuickConnect' {tags} -> tags) (\s@CreateQuickConnect' {} a -> s {tags = a} :: CreateQuickConnect) Prelude.. Lens.mapping Lens.coerced

-- | The description of the quick connect.
createQuickConnect_description :: Lens.Lens' CreateQuickConnect (Prelude.Maybe Prelude.Text)
createQuickConnect_description = Lens.lens (\CreateQuickConnect' {description} -> description) (\s@CreateQuickConnect' {} a -> s {description = a} :: CreateQuickConnect)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQuickConnectResponse'
            Prelude.<$> (x Data..?> "QuickConnectARN")
            Prelude.<*> (x Data..?> "QuickConnectId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateQuickConnect where
  hashWithSalt _salt CreateQuickConnect' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` quickConnectConfig

instance Prelude.NFData CreateQuickConnect where
  rnf CreateQuickConnect' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf quickConnectConfig

instance Data.ToHeaders CreateQuickConnect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateQuickConnect where
  toJSON CreateQuickConnect' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("QuickConnectConfig" Data..= quickConnectConfig)
          ]
      )

instance Data.ToPath CreateQuickConnect where
  toPath CreateQuickConnect' {..} =
    Prelude.mconcat
      ["/quick-connects/", Data.toBS instanceId]

instance Data.ToQuery CreateQuickConnect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateQuickConnectResponse' smart constructor.
data CreateQuickConnectResponse = CreateQuickConnectResponse'
  { -- | The Amazon Resource Name (ARN) for the quick connect.
    quickConnectARN :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Maybe Prelude.Text,
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
-- 'quickConnectARN', 'createQuickConnectResponse_quickConnectARN' - The Amazon Resource Name (ARN) for the quick connect.
--
-- 'quickConnectId', 'createQuickConnectResponse_quickConnectId' - The identifier for the quick connect.
--
-- 'httpStatus', 'createQuickConnectResponse_httpStatus' - The response's http status code.
newCreateQuickConnectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateQuickConnectResponse
newCreateQuickConnectResponse pHttpStatus_ =
  CreateQuickConnectResponse'
    { quickConnectARN =
        Prelude.Nothing,
      quickConnectId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the quick connect.
createQuickConnectResponse_quickConnectARN :: Lens.Lens' CreateQuickConnectResponse (Prelude.Maybe Prelude.Text)
createQuickConnectResponse_quickConnectARN = Lens.lens (\CreateQuickConnectResponse' {quickConnectARN} -> quickConnectARN) (\s@CreateQuickConnectResponse' {} a -> s {quickConnectARN = a} :: CreateQuickConnectResponse)

-- | The identifier for the quick connect.
createQuickConnectResponse_quickConnectId :: Lens.Lens' CreateQuickConnectResponse (Prelude.Maybe Prelude.Text)
createQuickConnectResponse_quickConnectId = Lens.lens (\CreateQuickConnectResponse' {quickConnectId} -> quickConnectId) (\s@CreateQuickConnectResponse' {} a -> s {quickConnectId = a} :: CreateQuickConnectResponse)

-- | The response's http status code.
createQuickConnectResponse_httpStatus :: Lens.Lens' CreateQuickConnectResponse Prelude.Int
createQuickConnectResponse_httpStatus = Lens.lens (\CreateQuickConnectResponse' {httpStatus} -> httpStatus) (\s@CreateQuickConnectResponse' {} a -> s {httpStatus = a} :: CreateQuickConnectResponse)

instance Prelude.NFData CreateQuickConnectResponse where
  rnf CreateQuickConnectResponse' {..} =
    Prelude.rnf quickConnectARN
      `Prelude.seq` Prelude.rnf quickConnectId
      `Prelude.seq` Prelude.rnf httpStatus
