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
-- Module      : Amazonka.SageMaker.CreateHub
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a hub.
module Amazonka.SageMaker.CreateHub
  ( -- * Creating a Request
    CreateHub (..),
    newCreateHub,

    -- * Request Lenses
    createHub_hubDisplayName,
    createHub_hubSearchKeywords,
    createHub_s3StorageConfig,
    createHub_tags,
    createHub_hubName,
    createHub_hubDescription,

    -- * Destructuring the Response
    CreateHubResponse (..),
    newCreateHubResponse,

    -- * Response Lenses
    createHubResponse_httpStatus,
    createHubResponse_hubArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateHub' smart constructor.
data CreateHub = CreateHub'
  { -- | The display name of the hub.
    hubDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords for the hub.
    hubSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 storage configuration for the hub.
    s3StorageConfig :: Prelude.Maybe HubS3StorageConfig,
    -- | Any tags to associate with the hub.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the hub to create.
    hubName :: Prelude.Text,
    -- | A description of the hub.
    hubDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHub' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubDisplayName', 'createHub_hubDisplayName' - The display name of the hub.
--
-- 'hubSearchKeywords', 'createHub_hubSearchKeywords' - The searchable keywords for the hub.
--
-- 's3StorageConfig', 'createHub_s3StorageConfig' - The Amazon S3 storage configuration for the hub.
--
-- 'tags', 'createHub_tags' - Any tags to associate with the hub.
--
-- 'hubName', 'createHub_hubName' - The name of the hub to create.
--
-- 'hubDescription', 'createHub_hubDescription' - A description of the hub.
newCreateHub ::
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubDescription'
  Prelude.Text ->
  CreateHub
newCreateHub pHubName_ pHubDescription_ =
  CreateHub'
    { hubDisplayName = Prelude.Nothing,
      hubSearchKeywords = Prelude.Nothing,
      s3StorageConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      hubName = pHubName_,
      hubDescription = pHubDescription_
    }

-- | The display name of the hub.
createHub_hubDisplayName :: Lens.Lens' CreateHub (Prelude.Maybe Prelude.Text)
createHub_hubDisplayName = Lens.lens (\CreateHub' {hubDisplayName} -> hubDisplayName) (\s@CreateHub' {} a -> s {hubDisplayName = a} :: CreateHub)

-- | The searchable keywords for the hub.
createHub_hubSearchKeywords :: Lens.Lens' CreateHub (Prelude.Maybe [Prelude.Text])
createHub_hubSearchKeywords = Lens.lens (\CreateHub' {hubSearchKeywords} -> hubSearchKeywords) (\s@CreateHub' {} a -> s {hubSearchKeywords = a} :: CreateHub) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 storage configuration for the hub.
createHub_s3StorageConfig :: Lens.Lens' CreateHub (Prelude.Maybe HubS3StorageConfig)
createHub_s3StorageConfig = Lens.lens (\CreateHub' {s3StorageConfig} -> s3StorageConfig) (\s@CreateHub' {} a -> s {s3StorageConfig = a} :: CreateHub)

-- | Any tags to associate with the hub.
createHub_tags :: Lens.Lens' CreateHub (Prelude.Maybe [Tag])
createHub_tags = Lens.lens (\CreateHub' {tags} -> tags) (\s@CreateHub' {} a -> s {tags = a} :: CreateHub) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hub to create.
createHub_hubName :: Lens.Lens' CreateHub Prelude.Text
createHub_hubName = Lens.lens (\CreateHub' {hubName} -> hubName) (\s@CreateHub' {} a -> s {hubName = a} :: CreateHub)

-- | A description of the hub.
createHub_hubDescription :: Lens.Lens' CreateHub Prelude.Text
createHub_hubDescription = Lens.lens (\CreateHub' {hubDescription} -> hubDescription) (\s@CreateHub' {} a -> s {hubDescription = a} :: CreateHub)

instance Core.AWSRequest CreateHub where
  type AWSResponse CreateHub = CreateHubResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHubResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HubArn")
      )

instance Prelude.Hashable CreateHub where
  hashWithSalt _salt CreateHub' {..} =
    _salt `Prelude.hashWithSalt` hubDisplayName
      `Prelude.hashWithSalt` hubSearchKeywords
      `Prelude.hashWithSalt` s3StorageConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubDescription

instance Prelude.NFData CreateHub where
  rnf CreateHub' {..} =
    Prelude.rnf hubDisplayName
      `Prelude.seq` Prelude.rnf hubSearchKeywords
      `Prelude.seq` Prelude.rnf s3StorageConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubDescription

instance Data.ToHeaders CreateHub where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateHub" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHub where
  toJSON CreateHub' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HubDisplayName" Data..=)
              Prelude.<$> hubDisplayName,
            ("HubSearchKeywords" Data..=)
              Prelude.<$> hubSearchKeywords,
            ("S3StorageConfig" Data..=)
              Prelude.<$> s3StorageConfig,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("HubName" Data..= hubName),
            Prelude.Just
              ("HubDescription" Data..= hubDescription)
          ]
      )

instance Data.ToPath CreateHub where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHub where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHubResponse' smart constructor.
data CreateHubResponse = CreateHubResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the hub.
    hubArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHubResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createHubResponse_httpStatus' - The response's http status code.
--
-- 'hubArn', 'createHubResponse_hubArn' - The Amazon Resource Name (ARN) of the hub.
newCreateHubResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hubArn'
  Prelude.Text ->
  CreateHubResponse
newCreateHubResponse pHttpStatus_ pHubArn_ =
  CreateHubResponse'
    { httpStatus = pHttpStatus_,
      hubArn = pHubArn_
    }

-- | The response's http status code.
createHubResponse_httpStatus :: Lens.Lens' CreateHubResponse Prelude.Int
createHubResponse_httpStatus = Lens.lens (\CreateHubResponse' {httpStatus} -> httpStatus) (\s@CreateHubResponse' {} a -> s {httpStatus = a} :: CreateHubResponse)

-- | The Amazon Resource Name (ARN) of the hub.
createHubResponse_hubArn :: Lens.Lens' CreateHubResponse Prelude.Text
createHubResponse_hubArn = Lens.lens (\CreateHubResponse' {hubArn} -> hubArn) (\s@CreateHubResponse' {} a -> s {hubArn = a} :: CreateHubResponse)

instance Prelude.NFData CreateHubResponse where
  rnf CreateHubResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hubArn
