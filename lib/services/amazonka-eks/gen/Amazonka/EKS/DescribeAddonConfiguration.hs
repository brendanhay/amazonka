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
-- Module      : Amazonka.EKS.DescribeAddonConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration options.
module Amazonka.EKS.DescribeAddonConfiguration
  ( -- * Creating a Request
    DescribeAddonConfiguration (..),
    newDescribeAddonConfiguration,

    -- * Request Lenses
    describeAddonConfiguration_addonName,
    describeAddonConfiguration_addonVersion,

    -- * Destructuring the Response
    DescribeAddonConfigurationResponse (..),
    newDescribeAddonConfigurationResponse,

    -- * Response Lenses
    describeAddonConfigurationResponse_addonName,
    describeAddonConfigurationResponse_addonVersion,
    describeAddonConfigurationResponse_configurationSchema,
    describeAddonConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAddonConfiguration' smart constructor.
data DescribeAddonConfiguration = DescribeAddonConfiguration'
  { -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
    -- .
    addonName :: Prelude.Text,
    -- | The version of the add-on. The version must match one of the versions
    -- returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
    -- .
    addonVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddonConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonName', 'describeAddonConfiguration_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
--
-- 'addonVersion', 'describeAddonConfiguration_addonVersion' - The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
newDescribeAddonConfiguration ::
  -- | 'addonName'
  Prelude.Text ->
  -- | 'addonVersion'
  Prelude.Text ->
  DescribeAddonConfiguration
newDescribeAddonConfiguration
  pAddonName_
  pAddonVersion_ =
    DescribeAddonConfiguration'
      { addonName =
          pAddonName_,
        addonVersion = pAddonVersion_
      }

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
describeAddonConfiguration_addonName :: Lens.Lens' DescribeAddonConfiguration Prelude.Text
describeAddonConfiguration_addonName = Lens.lens (\DescribeAddonConfiguration' {addonName} -> addonName) (\s@DescribeAddonConfiguration' {} a -> s {addonName = a} :: DescribeAddonConfiguration)

-- | The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
describeAddonConfiguration_addonVersion :: Lens.Lens' DescribeAddonConfiguration Prelude.Text
describeAddonConfiguration_addonVersion = Lens.lens (\DescribeAddonConfiguration' {addonVersion} -> addonVersion) (\s@DescribeAddonConfiguration' {} a -> s {addonVersion = a} :: DescribeAddonConfiguration)

instance Core.AWSRequest DescribeAddonConfiguration where
  type
    AWSResponse DescribeAddonConfiguration =
      DescribeAddonConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddonConfigurationResponse'
            Prelude.<$> (x Data..?> "addonName")
            Prelude.<*> (x Data..?> "addonVersion")
            Prelude.<*> (x Data..?> "configurationSchema")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddonConfiguration where
  hashWithSalt _salt DescribeAddonConfiguration' {..} =
    _salt `Prelude.hashWithSalt` addonName
      `Prelude.hashWithSalt` addonVersion

instance Prelude.NFData DescribeAddonConfiguration where
  rnf DescribeAddonConfiguration' {..} =
    Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf addonVersion

instance Data.ToHeaders DescribeAddonConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAddonConfiguration where
  toPath =
    Prelude.const "/addons/configuration-schemas"

instance Data.ToQuery DescribeAddonConfiguration where
  toQuery DescribeAddonConfiguration' {..} =
    Prelude.mconcat
      [ "addonName" Data.=: addonName,
        "addonVersion" Data.=: addonVersion
      ]

-- | /See:/ 'newDescribeAddonConfigurationResponse' smart constructor.
data DescribeAddonConfigurationResponse = DescribeAddonConfigurationResponse'
  { -- | The name of the add-on.
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The version of the add-on. The version must match one of the versions
    -- returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
    -- .
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | A JSON schema used to validate provided configuration values when
    -- creating or updating an addon.
    configurationSchema :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddonConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonName', 'describeAddonConfigurationResponse_addonName' - The name of the add-on.
--
-- 'addonVersion', 'describeAddonConfigurationResponse_addonVersion' - The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
--
-- 'configurationSchema', 'describeAddonConfigurationResponse_configurationSchema' - A JSON schema used to validate provided configuration values when
-- creating or updating an addon.
--
-- 'httpStatus', 'describeAddonConfigurationResponse_httpStatus' - The response's http status code.
newDescribeAddonConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAddonConfigurationResponse
newDescribeAddonConfigurationResponse pHttpStatus_ =
  DescribeAddonConfigurationResponse'
    { addonName =
        Prelude.Nothing,
      addonVersion = Prelude.Nothing,
      configurationSchema = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the add-on.
describeAddonConfigurationResponse_addonName :: Lens.Lens' DescribeAddonConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAddonConfigurationResponse_addonName = Lens.lens (\DescribeAddonConfigurationResponse' {addonName} -> addonName) (\s@DescribeAddonConfigurationResponse' {} a -> s {addonName = a} :: DescribeAddonConfigurationResponse)

-- | The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
describeAddonConfigurationResponse_addonVersion :: Lens.Lens' DescribeAddonConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAddonConfigurationResponse_addonVersion = Lens.lens (\DescribeAddonConfigurationResponse' {addonVersion} -> addonVersion) (\s@DescribeAddonConfigurationResponse' {} a -> s {addonVersion = a} :: DescribeAddonConfigurationResponse)

-- | A JSON schema used to validate provided configuration values when
-- creating or updating an addon.
describeAddonConfigurationResponse_configurationSchema :: Lens.Lens' DescribeAddonConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAddonConfigurationResponse_configurationSchema = Lens.lens (\DescribeAddonConfigurationResponse' {configurationSchema} -> configurationSchema) (\s@DescribeAddonConfigurationResponse' {} a -> s {configurationSchema = a} :: DescribeAddonConfigurationResponse)

-- | The response's http status code.
describeAddonConfigurationResponse_httpStatus :: Lens.Lens' DescribeAddonConfigurationResponse Prelude.Int
describeAddonConfigurationResponse_httpStatus = Lens.lens (\DescribeAddonConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeAddonConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeAddonConfigurationResponse)

instance
  Prelude.NFData
    DescribeAddonConfigurationResponse
  where
  rnf DescribeAddonConfigurationResponse' {..} =
    Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf addonVersion
      `Prelude.seq` Prelude.rnf configurationSchema
      `Prelude.seq` Prelude.rnf httpStatus
