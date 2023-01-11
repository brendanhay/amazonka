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
-- Module      : Amazonka.AppRunner.DescribeAutoScalingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a full description of an App Runner automatic scaling
-- configuration resource.
module Amazonka.AppRunner.DescribeAutoScalingConfiguration
  ( -- * Creating a Request
    DescribeAutoScalingConfiguration (..),
    newDescribeAutoScalingConfiguration,

    -- * Request Lenses
    describeAutoScalingConfiguration_autoScalingConfigurationArn,

    -- * Destructuring the Response
    DescribeAutoScalingConfigurationResponse (..),
    newDescribeAutoScalingConfigurationResponse,

    -- * Response Lenses
    describeAutoScalingConfigurationResponse_httpStatus,
    describeAutoScalingConfigurationResponse_autoScalingConfiguration,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAutoScalingConfiguration' smart constructor.
data DescribeAutoScalingConfiguration = DescribeAutoScalingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the App Runner auto scaling
    -- configuration that you want a description for.
    --
    -- The ARN can be a full auto scaling configuration ARN, or a partial ARN
    -- ending with either @...\/name @ or @...\/name\/revision @. If a revision
    -- isn\'t specified, the latest active revision is described.
    autoScalingConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfigurationArn', 'describeAutoScalingConfiguration_autoScalingConfigurationArn' - The Amazon Resource Name (ARN) of the App Runner auto scaling
-- configuration that you want a description for.
--
-- The ARN can be a full auto scaling configuration ARN, or a partial ARN
-- ending with either @...\/name @ or @...\/name\/revision @. If a revision
-- isn\'t specified, the latest active revision is described.
newDescribeAutoScalingConfiguration ::
  -- | 'autoScalingConfigurationArn'
  Prelude.Text ->
  DescribeAutoScalingConfiguration
newDescribeAutoScalingConfiguration
  pAutoScalingConfigurationArn_ =
    DescribeAutoScalingConfiguration'
      { autoScalingConfigurationArn =
          pAutoScalingConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the App Runner auto scaling
-- configuration that you want a description for.
--
-- The ARN can be a full auto scaling configuration ARN, or a partial ARN
-- ending with either @...\/name @ or @...\/name\/revision @. If a revision
-- isn\'t specified, the latest active revision is described.
describeAutoScalingConfiguration_autoScalingConfigurationArn :: Lens.Lens' DescribeAutoScalingConfiguration Prelude.Text
describeAutoScalingConfiguration_autoScalingConfigurationArn = Lens.lens (\DescribeAutoScalingConfiguration' {autoScalingConfigurationArn} -> autoScalingConfigurationArn) (\s@DescribeAutoScalingConfiguration' {} a -> s {autoScalingConfigurationArn = a} :: DescribeAutoScalingConfiguration)

instance
  Core.AWSRequest
    DescribeAutoScalingConfiguration
  where
  type
    AWSResponse DescribeAutoScalingConfiguration =
      DescribeAutoScalingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutoScalingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AutoScalingConfiguration")
      )

instance
  Prelude.Hashable
    DescribeAutoScalingConfiguration
  where
  hashWithSalt
    _salt
    DescribeAutoScalingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoScalingConfigurationArn

instance
  Prelude.NFData
    DescribeAutoScalingConfiguration
  where
  rnf DescribeAutoScalingConfiguration' {..} =
    Prelude.rnf autoScalingConfigurationArn

instance
  Data.ToHeaders
    DescribeAutoScalingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DescribeAutoScalingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAutoScalingConfiguration where
  toJSON DescribeAutoScalingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AutoScalingConfigurationArn"
                  Data..= autoScalingConfigurationArn
              )
          ]
      )

instance Data.ToPath DescribeAutoScalingConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAutoScalingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutoScalingConfigurationResponse' smart constructor.
data DescribeAutoScalingConfigurationResponse = DescribeAutoScalingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the App Runner auto scaling configuration that you
    -- specified in this request.
    autoScalingConfiguration :: AutoScalingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoScalingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAutoScalingConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'autoScalingConfiguration', 'describeAutoScalingConfigurationResponse_autoScalingConfiguration' - A full description of the App Runner auto scaling configuration that you
-- specified in this request.
newDescribeAutoScalingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoScalingConfiguration'
  AutoScalingConfiguration ->
  DescribeAutoScalingConfigurationResponse
newDescribeAutoScalingConfigurationResponse
  pHttpStatus_
  pAutoScalingConfiguration_ =
    DescribeAutoScalingConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        autoScalingConfiguration =
          pAutoScalingConfiguration_
      }

-- | The response's http status code.
describeAutoScalingConfigurationResponse_httpStatus :: Lens.Lens' DescribeAutoScalingConfigurationResponse Prelude.Int
describeAutoScalingConfigurationResponse_httpStatus = Lens.lens (\DescribeAutoScalingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoScalingConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeAutoScalingConfigurationResponse)

-- | A full description of the App Runner auto scaling configuration that you
-- specified in this request.
describeAutoScalingConfigurationResponse_autoScalingConfiguration :: Lens.Lens' DescribeAutoScalingConfigurationResponse AutoScalingConfiguration
describeAutoScalingConfigurationResponse_autoScalingConfiguration = Lens.lens (\DescribeAutoScalingConfigurationResponse' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@DescribeAutoScalingConfigurationResponse' {} a -> s {autoScalingConfiguration = a} :: DescribeAutoScalingConfigurationResponse)

instance
  Prelude.NFData
    DescribeAutoScalingConfigurationResponse
  where
  rnf DescribeAutoScalingConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoScalingConfiguration
