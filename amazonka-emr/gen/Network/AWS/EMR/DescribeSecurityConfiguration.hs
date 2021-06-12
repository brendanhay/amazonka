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
-- Module      : Network.AWS.EMR.DescribeSecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details of a security configuration by returning the
-- configuration JSON.
module Network.AWS.EMR.DescribeSecurityConfiguration
  ( -- * Creating a Request
    DescribeSecurityConfiguration (..),
    newDescribeSecurityConfiguration,

    -- * Request Lenses
    describeSecurityConfiguration_name,

    -- * Destructuring the Response
    DescribeSecurityConfigurationResponse (..),
    newDescribeSecurityConfigurationResponse,

    -- * Response Lenses
    describeSecurityConfigurationResponse_securityConfiguration,
    describeSecurityConfigurationResponse_name,
    describeSecurityConfigurationResponse_creationDateTime,
    describeSecurityConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSecurityConfiguration' smart constructor.
data DescribeSecurityConfiguration = DescribeSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeSecurityConfiguration_name' - The name of the security configuration.
newDescribeSecurityConfiguration ::
  -- | 'name'
  Core.Text ->
  DescribeSecurityConfiguration
newDescribeSecurityConfiguration pName_ =
  DescribeSecurityConfiguration' {name = pName_}

-- | The name of the security configuration.
describeSecurityConfiguration_name :: Lens.Lens' DescribeSecurityConfiguration Core.Text
describeSecurityConfiguration_name = Lens.lens (\DescribeSecurityConfiguration' {name} -> name) (\s@DescribeSecurityConfiguration' {} a -> s {name = a} :: DescribeSecurityConfiguration)

instance
  Core.AWSRequest
    DescribeSecurityConfiguration
  where
  type
    AWSResponse DescribeSecurityConfiguration =
      DescribeSecurityConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityConfigurationResponse'
            Core.<$> (x Core..?> "SecurityConfiguration")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "CreationDateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSecurityConfiguration

instance Core.NFData DescribeSecurityConfiguration

instance Core.ToHeaders DescribeSecurityConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeSecurityConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSecurityConfiguration where
  toJSON DescribeSecurityConfiguration' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DescribeSecurityConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSecurityConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSecurityConfigurationResponse' smart constructor.
data DescribeSecurityConfigurationResponse = DescribeSecurityConfigurationResponse'
  { -- | The security configuration details in JSON format.
    securityConfiguration :: Core.Maybe Core.Text,
    -- | The name of the security configuration.
    name :: Core.Maybe Core.Text,
    -- | The date and time the security configuration was created
    creationDateTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecurityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'describeSecurityConfigurationResponse_securityConfiguration' - The security configuration details in JSON format.
--
-- 'name', 'describeSecurityConfigurationResponse_name' - The name of the security configuration.
--
-- 'creationDateTime', 'describeSecurityConfigurationResponse_creationDateTime' - The date and time the security configuration was created
--
-- 'httpStatus', 'describeSecurityConfigurationResponse_httpStatus' - The response's http status code.
newDescribeSecurityConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSecurityConfigurationResponse
newDescribeSecurityConfigurationResponse pHttpStatus_ =
  DescribeSecurityConfigurationResponse'
    { securityConfiguration =
        Core.Nothing,
      name = Core.Nothing,
      creationDateTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The security configuration details in JSON format.
describeSecurityConfigurationResponse_securityConfiguration :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Core.Text)
describeSecurityConfigurationResponse_securityConfiguration = Lens.lens (\DescribeSecurityConfigurationResponse' {securityConfiguration} -> securityConfiguration) (\s@DescribeSecurityConfigurationResponse' {} a -> s {securityConfiguration = a} :: DescribeSecurityConfigurationResponse)

-- | The name of the security configuration.
describeSecurityConfigurationResponse_name :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Core.Text)
describeSecurityConfigurationResponse_name = Lens.lens (\DescribeSecurityConfigurationResponse' {name} -> name) (\s@DescribeSecurityConfigurationResponse' {} a -> s {name = a} :: DescribeSecurityConfigurationResponse)

-- | The date and time the security configuration was created
describeSecurityConfigurationResponse_creationDateTime :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Core.UTCTime)
describeSecurityConfigurationResponse_creationDateTime = Lens.lens (\DescribeSecurityConfigurationResponse' {creationDateTime} -> creationDateTime) (\s@DescribeSecurityConfigurationResponse' {} a -> s {creationDateTime = a} :: DescribeSecurityConfigurationResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeSecurityConfigurationResponse_httpStatus :: Lens.Lens' DescribeSecurityConfigurationResponse Core.Int
describeSecurityConfigurationResponse_httpStatus = Lens.lens (\DescribeSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeSecurityConfigurationResponse)

instance
  Core.NFData
    DescribeSecurityConfigurationResponse
