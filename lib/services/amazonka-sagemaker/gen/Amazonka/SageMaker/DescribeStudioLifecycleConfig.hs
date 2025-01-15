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
-- Module      : Amazonka.SageMaker.DescribeStudioLifecycleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Studio Lifecycle Configuration.
module Amazonka.SageMaker.DescribeStudioLifecycleConfig
  ( -- * Creating a Request
    DescribeStudioLifecycleConfig (..),
    newDescribeStudioLifecycleConfig,

    -- * Request Lenses
    describeStudioLifecycleConfig_studioLifecycleConfigName,

    -- * Destructuring the Response
    DescribeStudioLifecycleConfigResponse (..),
    newDescribeStudioLifecycleConfigResponse,

    -- * Response Lenses
    describeStudioLifecycleConfigResponse_creationTime,
    describeStudioLifecycleConfigResponse_lastModifiedTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigContent,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeStudioLifecycleConfig' smart constructor.
data DescribeStudioLifecycleConfig = DescribeStudioLifecycleConfig'
  { -- | The name of the Studio Lifecycle Configuration to describe.
    studioLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStudioLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioLifecycleConfigName', 'describeStudioLifecycleConfig_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration to describe.
newDescribeStudioLifecycleConfig ::
  -- | 'studioLifecycleConfigName'
  Prelude.Text ->
  DescribeStudioLifecycleConfig
newDescribeStudioLifecycleConfig
  pStudioLifecycleConfigName_ =
    DescribeStudioLifecycleConfig'
      { studioLifecycleConfigName =
          pStudioLifecycleConfigName_
      }

-- | The name of the Studio Lifecycle Configuration to describe.
describeStudioLifecycleConfig_studioLifecycleConfigName :: Lens.Lens' DescribeStudioLifecycleConfig Prelude.Text
describeStudioLifecycleConfig_studioLifecycleConfigName = Lens.lens (\DescribeStudioLifecycleConfig' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@DescribeStudioLifecycleConfig' {} a -> s {studioLifecycleConfigName = a} :: DescribeStudioLifecycleConfig)

instance
  Core.AWSRequest
    DescribeStudioLifecycleConfig
  where
  type
    AWSResponse DescribeStudioLifecycleConfig =
      DescribeStudioLifecycleConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStudioLifecycleConfigResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "StudioLifecycleConfigAppType")
            Prelude.<*> (x Data..?> "StudioLifecycleConfigArn")
            Prelude.<*> (x Data..?> "StudioLifecycleConfigContent")
            Prelude.<*> (x Data..?> "StudioLifecycleConfigName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeStudioLifecycleConfig
  where
  hashWithSalt _salt DescribeStudioLifecycleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` studioLifecycleConfigName

instance Prelude.NFData DescribeStudioLifecycleConfig where
  rnf DescribeStudioLifecycleConfig' {..} =
    Prelude.rnf studioLifecycleConfigName

instance Data.ToHeaders DescribeStudioLifecycleConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeStudioLifecycleConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStudioLifecycleConfig where
  toJSON DescribeStudioLifecycleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StudioLifecycleConfigName"
                  Data..= studioLifecycleConfigName
              )
          ]
      )

instance Data.ToPath DescribeStudioLifecycleConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStudioLifecycleConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStudioLifecycleConfigResponse' smart constructor.
data DescribeStudioLifecycleConfigResponse = DescribeStudioLifecycleConfigResponse'
  { -- | The creation time of the Studio Lifecycle Configuration.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | This value is equivalent to CreationTime because Studio Lifecycle
    -- Configurations are immutable.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The App type that the Lifecycle Configuration is attached to.
    studioLifecycleConfigAppType :: Prelude.Maybe StudioLifecycleConfigAppType,
    -- | The ARN of the Lifecycle Configuration to describe.
    studioLifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The content of your Studio Lifecycle Configuration script.
    studioLifecycleConfigContent :: Prelude.Maybe Prelude.Text,
    -- | The name of the Studio Lifecycle Configuration that is described.
    studioLifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStudioLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeStudioLifecycleConfigResponse_creationTime' - The creation time of the Studio Lifecycle Configuration.
--
-- 'lastModifiedTime', 'describeStudioLifecycleConfigResponse_lastModifiedTime' - This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
--
-- 'studioLifecycleConfigAppType', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType' - The App type that the Lifecycle Configuration is attached to.
--
-- 'studioLifecycleConfigArn', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigArn' - The ARN of the Lifecycle Configuration to describe.
--
-- 'studioLifecycleConfigContent', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigContent' - The content of your Studio Lifecycle Configuration script.
--
-- 'studioLifecycleConfigName', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration that is described.
--
-- 'httpStatus', 'describeStudioLifecycleConfigResponse_httpStatus' - The response's http status code.
newDescribeStudioLifecycleConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStudioLifecycleConfigResponse
newDescribeStudioLifecycleConfigResponse pHttpStatus_ =
  DescribeStudioLifecycleConfigResponse'
    { creationTime =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      studioLifecycleConfigAppType =
        Prelude.Nothing,
      studioLifecycleConfigArn =
        Prelude.Nothing,
      studioLifecycleConfigContent =
        Prelude.Nothing,
      studioLifecycleConfigName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation time of the Studio Lifecycle Configuration.
describeStudioLifecycleConfigResponse_creationTime :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeStudioLifecycleConfigResponse_creationTime = Lens.lens (\DescribeStudioLifecycleConfigResponse' {creationTime} -> creationTime) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {creationTime = a} :: DescribeStudioLifecycleConfigResponse) Prelude.. Lens.mapping Data._Time

-- | This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
describeStudioLifecycleConfigResponse_lastModifiedTime :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeStudioLifecycleConfigResponse_lastModifiedTime = Lens.lens (\DescribeStudioLifecycleConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {lastModifiedTime = a} :: DescribeStudioLifecycleConfigResponse) Prelude.. Lens.mapping Data._Time

-- | The App type that the Lifecycle Configuration is attached to.
describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe StudioLifecycleConfigAppType)
describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigAppType} -> studioLifecycleConfigAppType) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigAppType = a} :: DescribeStudioLifecycleConfigResponse)

-- | The ARN of the Lifecycle Configuration to describe.
describeStudioLifecycleConfigResponse_studioLifecycleConfigArn :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeStudioLifecycleConfigResponse_studioLifecycleConfigArn = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigArn} -> studioLifecycleConfigArn) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigArn = a} :: DescribeStudioLifecycleConfigResponse)

-- | The content of your Studio Lifecycle Configuration script.
describeStudioLifecycleConfigResponse_studioLifecycleConfigContent :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeStudioLifecycleConfigResponse_studioLifecycleConfigContent = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigContent} -> studioLifecycleConfigContent) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigContent = a} :: DescribeStudioLifecycleConfigResponse)

-- | The name of the Studio Lifecycle Configuration that is described.
describeStudioLifecycleConfigResponse_studioLifecycleConfigName :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeStudioLifecycleConfigResponse_studioLifecycleConfigName = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigName = a} :: DescribeStudioLifecycleConfigResponse)

-- | The response's http status code.
describeStudioLifecycleConfigResponse_httpStatus :: Lens.Lens' DescribeStudioLifecycleConfigResponse Prelude.Int
describeStudioLifecycleConfigResponse_httpStatus = Lens.lens (\DescribeStudioLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {httpStatus = a} :: DescribeStudioLifecycleConfigResponse)

instance
  Prelude.NFData
    DescribeStudioLifecycleConfigResponse
  where
  rnf DescribeStudioLifecycleConfigResponse' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf lastModifiedTime `Prelude.seq`
        Prelude.rnf studioLifecycleConfigAppType `Prelude.seq`
          Prelude.rnf studioLifecycleConfigArn `Prelude.seq`
            Prelude.rnf studioLifecycleConfigContent `Prelude.seq`
              Prelude.rnf studioLifecycleConfigName `Prelude.seq`
                Prelude.rnf httpStatus
