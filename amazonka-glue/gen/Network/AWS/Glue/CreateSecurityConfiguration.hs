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
-- Module      : Network.AWS.Glue.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new security configuration. A security configuration is a set
-- of security properties that can be used by AWS Glue. You can use a
-- security configuration to encrypt data at rest. For information about
-- using security configurations in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/encryption-security-configuration.html Encrypting Data Written by Crawlers, Jobs, and Development Endpoints>.
module Network.AWS.Glue.CreateSecurityConfiguration
  ( -- * Creating a Request
    CreateSecurityConfiguration (..),
    newCreateSecurityConfiguration,

    -- * Request Lenses
    createSecurityConfiguration_name,
    createSecurityConfiguration_encryptionConfiguration,

    -- * Destructuring the Response
    CreateSecurityConfigurationResponse (..),
    newCreateSecurityConfigurationResponse,

    -- * Response Lenses
    createSecurityConfigurationResponse_createdTimestamp,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The name for the new security configuration.
    name :: Core.Text,
    -- | The encryption configuration for the new security configuration.
    encryptionConfiguration :: EncryptionConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSecurityConfiguration_name' - The name for the new security configuration.
--
-- 'encryptionConfiguration', 'createSecurityConfiguration_encryptionConfiguration' - The encryption configuration for the new security configuration.
newCreateSecurityConfiguration ::
  -- | 'name'
  Core.Text ->
  -- | 'encryptionConfiguration'
  EncryptionConfiguration ->
  CreateSecurityConfiguration
newCreateSecurityConfiguration
  pName_
  pEncryptionConfiguration_ =
    CreateSecurityConfiguration'
      { name = pName_,
        encryptionConfiguration =
          pEncryptionConfiguration_
      }

-- | The name for the new security configuration.
createSecurityConfiguration_name :: Lens.Lens' CreateSecurityConfiguration Core.Text
createSecurityConfiguration_name = Lens.lens (\CreateSecurityConfiguration' {name} -> name) (\s@CreateSecurityConfiguration' {} a -> s {name = a} :: CreateSecurityConfiguration)

-- | The encryption configuration for the new security configuration.
createSecurityConfiguration_encryptionConfiguration :: Lens.Lens' CreateSecurityConfiguration EncryptionConfiguration
createSecurityConfiguration_encryptionConfiguration = Lens.lens (\CreateSecurityConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateSecurityConfiguration' {} a -> s {encryptionConfiguration = a} :: CreateSecurityConfiguration)

instance Core.AWSRequest CreateSecurityConfiguration where
  type
    AWSResponse CreateSecurityConfiguration =
      CreateSecurityConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            Core.<$> (x Core..?> "CreatedTimestamp")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSecurityConfiguration

instance Core.NFData CreateSecurityConfiguration

instance Core.ToHeaders CreateSecurityConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.CreateSecurityConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just
              ( "EncryptionConfiguration"
                  Core..= encryptionConfiguration
              )
          ]
      )

instance Core.ToPath CreateSecurityConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery CreateSecurityConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The time at which the new security configuration was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The name assigned to the new security configuration.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSecurityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'createSecurityConfigurationResponse_createdTimestamp' - The time at which the new security configuration was created.
--
-- 'name', 'createSecurityConfigurationResponse_name' - The name assigned to the new security configuration.
--
-- 'httpStatus', 'createSecurityConfigurationResponse_httpStatus' - The response's http status code.
newCreateSecurityConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSecurityConfigurationResponse
newCreateSecurityConfigurationResponse pHttpStatus_ =
  CreateSecurityConfigurationResponse'
    { createdTimestamp =
        Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the new security configuration was created.
createSecurityConfigurationResponse_createdTimestamp :: Lens.Lens' CreateSecurityConfigurationResponse (Core.Maybe Core.UTCTime)
createSecurityConfigurationResponse_createdTimestamp = Lens.lens (\CreateSecurityConfigurationResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateSecurityConfigurationResponse' {} a -> s {createdTimestamp = a} :: CreateSecurityConfigurationResponse) Core.. Lens.mapping Core._Time

-- | The name assigned to the new security configuration.
createSecurityConfigurationResponse_name :: Lens.Lens' CreateSecurityConfigurationResponse (Core.Maybe Core.Text)
createSecurityConfigurationResponse_name = Lens.lens (\CreateSecurityConfigurationResponse' {name} -> name) (\s@CreateSecurityConfigurationResponse' {} a -> s {name = a} :: CreateSecurityConfigurationResponse)

-- | The response's http status code.
createSecurityConfigurationResponse_httpStatus :: Lens.Lens' CreateSecurityConfigurationResponse Core.Int
createSecurityConfigurationResponse_httpStatus = Lens.lens (\CreateSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: CreateSecurityConfigurationResponse)

instance
  Core.NFData
    CreateSecurityConfigurationResponse
