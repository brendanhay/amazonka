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
-- Module      : Amazonka.EMR.DescribeSecurityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details of a security configuration by returning the
-- configuration JSON.
module Amazonka.EMR.DescribeSecurityConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSecurityConfiguration' smart constructor.
data DescribeSecurityConfiguration = DescribeSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeSecurityConfiguration
newDescribeSecurityConfiguration pName_ =
  DescribeSecurityConfiguration' {name = pName_}

-- | The name of the security configuration.
describeSecurityConfiguration_name :: Lens.Lens' DescribeSecurityConfiguration Prelude.Text
describeSecurityConfiguration_name = Lens.lens (\DescribeSecurityConfiguration' {name} -> name) (\s@DescribeSecurityConfiguration' {} a -> s {name = a} :: DescribeSecurityConfiguration)

instance
  Core.AWSRequest
    DescribeSecurityConfiguration
  where
  type
    AWSResponse DescribeSecurityConfiguration =
      DescribeSecurityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityConfigurationResponse'
            Prelude.<$> (x Core..?> "SecurityConfiguration")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "CreationDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSecurityConfiguration
  where
  hashWithSalt _salt DescribeSecurityConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeSecurityConfiguration where
  rnf DescribeSecurityConfiguration' {..} =
    Prelude.rnf name

instance Core.ToHeaders DescribeSecurityConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeSecurityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSecurityConfiguration where
  toJSON DescribeSecurityConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DescribeSecurityConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSecurityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSecurityConfigurationResponse' smart constructor.
data DescribeSecurityConfigurationResponse = DescribeSecurityConfigurationResponse'
  { -- | The security configuration details in JSON format.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The name of the security configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time the security configuration was created
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSecurityConfigurationResponse
newDescribeSecurityConfigurationResponse pHttpStatus_ =
  DescribeSecurityConfigurationResponse'
    { securityConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The security configuration details in JSON format.
describeSecurityConfigurationResponse_securityConfiguration :: Lens.Lens' DescribeSecurityConfigurationResponse (Prelude.Maybe Prelude.Text)
describeSecurityConfigurationResponse_securityConfiguration = Lens.lens (\DescribeSecurityConfigurationResponse' {securityConfiguration} -> securityConfiguration) (\s@DescribeSecurityConfigurationResponse' {} a -> s {securityConfiguration = a} :: DescribeSecurityConfigurationResponse)

-- | The name of the security configuration.
describeSecurityConfigurationResponse_name :: Lens.Lens' DescribeSecurityConfigurationResponse (Prelude.Maybe Prelude.Text)
describeSecurityConfigurationResponse_name = Lens.lens (\DescribeSecurityConfigurationResponse' {name} -> name) (\s@DescribeSecurityConfigurationResponse' {} a -> s {name = a} :: DescribeSecurityConfigurationResponse)

-- | The date and time the security configuration was created
describeSecurityConfigurationResponse_creationDateTime :: Lens.Lens' DescribeSecurityConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeSecurityConfigurationResponse_creationDateTime = Lens.lens (\DescribeSecurityConfigurationResponse' {creationDateTime} -> creationDateTime) (\s@DescribeSecurityConfigurationResponse' {} a -> s {creationDateTime = a} :: DescribeSecurityConfigurationResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeSecurityConfigurationResponse_httpStatus :: Lens.Lens' DescribeSecurityConfigurationResponse Prelude.Int
describeSecurityConfigurationResponse_httpStatus = Lens.lens (\DescribeSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeSecurityConfigurationResponse)

instance
  Prelude.NFData
    DescribeSecurityConfigurationResponse
  where
  rnf DescribeSecurityConfigurationResponse' {..} =
    Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf httpStatus
