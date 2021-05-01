{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The name for the new security configuration.
    name :: Prelude.Text,
    -- | The encryption configuration for the new security configuration.
    encryptionConfiguration :: EncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
createSecurityConfiguration_name :: Lens.Lens' CreateSecurityConfiguration Prelude.Text
createSecurityConfiguration_name = Lens.lens (\CreateSecurityConfiguration' {name} -> name) (\s@CreateSecurityConfiguration' {} a -> s {name = a} :: CreateSecurityConfiguration)

-- | The encryption configuration for the new security configuration.
createSecurityConfiguration_encryptionConfiguration :: Lens.Lens' CreateSecurityConfiguration EncryptionConfiguration
createSecurityConfiguration_encryptionConfiguration = Lens.lens (\CreateSecurityConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateSecurityConfiguration' {} a -> s {encryptionConfiguration = a} :: CreateSecurityConfiguration)

instance
  Prelude.AWSRequest
    CreateSecurityConfiguration
  where
  type
    Rs CreateSecurityConfiguration =
      CreateSecurityConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            Prelude.<$> (x Prelude..?> "CreatedTimestamp")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecurityConfiguration

instance Prelude.NFData CreateSecurityConfiguration

instance
  Prelude.ToHeaders
    CreateSecurityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.CreateSecurityConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ( "EncryptionConfiguration"
                  Prelude..= encryptionConfiguration
              )
          ]
      )

instance Prelude.ToPath CreateSecurityConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSecurityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The time at which the new security configuration was created.
    createdTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The name assigned to the new security configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateSecurityConfigurationResponse
newCreateSecurityConfigurationResponse pHttpStatus_ =
  CreateSecurityConfigurationResponse'
    { createdTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the new security configuration was created.
createSecurityConfigurationResponse_createdTimestamp :: Lens.Lens' CreateSecurityConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
createSecurityConfigurationResponse_createdTimestamp = Lens.lens (\CreateSecurityConfigurationResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateSecurityConfigurationResponse' {} a -> s {createdTimestamp = a} :: CreateSecurityConfigurationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The name assigned to the new security configuration.
createSecurityConfigurationResponse_name :: Lens.Lens' CreateSecurityConfigurationResponse (Prelude.Maybe Prelude.Text)
createSecurityConfigurationResponse_name = Lens.lens (\CreateSecurityConfigurationResponse' {name} -> name) (\s@CreateSecurityConfigurationResponse' {} a -> s {name = a} :: CreateSecurityConfigurationResponse)

-- | The response's http status code.
createSecurityConfigurationResponse_httpStatus :: Lens.Lens' CreateSecurityConfigurationResponse Prelude.Int
createSecurityConfigurationResponse_httpStatus = Lens.lens (\CreateSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: CreateSecurityConfigurationResponse)

instance
  Prelude.NFData
    CreateSecurityConfigurationResponse
