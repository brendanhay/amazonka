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
-- Module      : Network.AWS.EMR.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security configuration, which is stored in the service and can
-- be specified when a cluster is created.
module Network.AWS.EMR.CreateSecurityConfiguration
  ( -- * Creating a Request
    CreateSecurityConfiguration (..),
    newCreateSecurityConfiguration,

    -- * Request Lenses
    createSecurityConfiguration_name,
    createSecurityConfiguration_securityConfiguration,

    -- * Destructuring the Response
    CreateSecurityConfigurationResponse (..),
    newCreateSecurityConfigurationResponse,

    -- * Response Lenses
    createSecurityConfigurationResponse_httpStatus,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_creationDateTime,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Prelude.Text,
    -- | The security configuration details in JSON format. For JSON parameters
    -- and examples, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security>
    -- in the /Amazon EMR Management Guide/.
    securityConfiguration :: Prelude.Text
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
-- 'name', 'createSecurityConfiguration_name' - The name of the security configuration.
--
-- 'securityConfiguration', 'createSecurityConfiguration_securityConfiguration' - The security configuration details in JSON format. For JSON parameters
-- and examples, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security>
-- in the /Amazon EMR Management Guide/.
newCreateSecurityConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'securityConfiguration'
  Prelude.Text ->
  CreateSecurityConfiguration
newCreateSecurityConfiguration
  pName_
  pSecurityConfiguration_ =
    CreateSecurityConfiguration'
      { name = pName_,
        securityConfiguration =
          pSecurityConfiguration_
      }

-- | The name of the security configuration.
createSecurityConfiguration_name :: Lens.Lens' CreateSecurityConfiguration Prelude.Text
createSecurityConfiguration_name = Lens.lens (\CreateSecurityConfiguration' {name} -> name) (\s@CreateSecurityConfiguration' {} a -> s {name = a} :: CreateSecurityConfiguration)

-- | The security configuration details in JSON format. For JSON parameters
-- and examples, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security>
-- in the /Amazon EMR Management Guide/.
createSecurityConfiguration_securityConfiguration :: Lens.Lens' CreateSecurityConfiguration Prelude.Text
createSecurityConfiguration_securityConfiguration = Lens.lens (\CreateSecurityConfiguration' {securityConfiguration} -> securityConfiguration) (\s@CreateSecurityConfiguration' {} a -> s {securityConfiguration = a} :: CreateSecurityConfiguration)

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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Name")
            Prelude.<*> (x Prelude..:> "CreationDateTime")
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
              Prelude.=# ( "ElasticMapReduce.CreateSecurityConfiguration" ::
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
              ( "SecurityConfiguration"
                  Prelude..= securityConfiguration
              )
          ]
      )

instance Prelude.ToPath CreateSecurityConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSecurityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the security configuration.
    name :: Prelude.Text,
    -- | The date and time the security configuration was created.
    creationDateTime :: Prelude.POSIX
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
-- 'httpStatus', 'createSecurityConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createSecurityConfigurationResponse_name' - The name of the security configuration.
--
-- 'creationDateTime', 'createSecurityConfigurationResponse_creationDateTime' - The date and time the security configuration was created.
newCreateSecurityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  CreateSecurityConfigurationResponse
newCreateSecurityConfigurationResponse
  pHttpStatus_
  pName_
  pCreationDateTime_ =
    CreateSecurityConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        creationDateTime =
          Prelude._Time
            Lens.# pCreationDateTime_
      }

-- | The response's http status code.
createSecurityConfigurationResponse_httpStatus :: Lens.Lens' CreateSecurityConfigurationResponse Prelude.Int
createSecurityConfigurationResponse_httpStatus = Lens.lens (\CreateSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: CreateSecurityConfigurationResponse)

-- | The name of the security configuration.
createSecurityConfigurationResponse_name :: Lens.Lens' CreateSecurityConfigurationResponse Prelude.Text
createSecurityConfigurationResponse_name = Lens.lens (\CreateSecurityConfigurationResponse' {name} -> name) (\s@CreateSecurityConfigurationResponse' {} a -> s {name = a} :: CreateSecurityConfigurationResponse)

-- | The date and time the security configuration was created.
createSecurityConfigurationResponse_creationDateTime :: Lens.Lens' CreateSecurityConfigurationResponse Prelude.UTCTime
createSecurityConfigurationResponse_creationDateTime = Lens.lens (\CreateSecurityConfigurationResponse' {creationDateTime} -> creationDateTime) (\s@CreateSecurityConfigurationResponse' {} a -> s {creationDateTime = a} :: CreateSecurityConfigurationResponse) Prelude.. Prelude._Time

instance
  Prelude.NFData
    CreateSecurityConfigurationResponse
