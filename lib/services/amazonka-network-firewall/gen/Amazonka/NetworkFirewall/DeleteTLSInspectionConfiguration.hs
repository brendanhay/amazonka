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
-- Module      : Amazonka.NetworkFirewall.DeleteTLSInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TLSInspectionConfiguration.
module Amazonka.NetworkFirewall.DeleteTLSInspectionConfiguration
  ( -- * Creating a Request
    DeleteTLSInspectionConfiguration (..),
    newDeleteTLSInspectionConfiguration,

    -- * Request Lenses
    deleteTLSInspectionConfiguration_tLSInspectionConfigurationArn,
    deleteTLSInspectionConfiguration_tLSInspectionConfigurationName,

    -- * Destructuring the Response
    DeleteTLSInspectionConfigurationResponse (..),
    newDeleteTLSInspectionConfigurationResponse,

    -- * Response Lenses
    deleteTLSInspectionConfigurationResponse_httpStatus,
    deleteTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTLSInspectionConfiguration' smart constructor.
data DeleteTLSInspectionConfiguration = DeleteTLSInspectionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
    --
    -- You must specify the ARN or the name, and you can specify both.
    tLSInspectionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the TLS inspection configuration. You can\'t
    -- change the name of a TLS inspection configuration after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    tLSInspectionConfigurationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTLSInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tLSInspectionConfigurationArn', 'deleteTLSInspectionConfiguration_tLSInspectionConfigurationArn' - The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'tLSInspectionConfigurationName', 'deleteTLSInspectionConfiguration_tLSInspectionConfigurationName' - The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDeleteTLSInspectionConfiguration ::
  DeleteTLSInspectionConfiguration
newDeleteTLSInspectionConfiguration =
  DeleteTLSInspectionConfiguration'
    { tLSInspectionConfigurationArn =
        Prelude.Nothing,
      tLSInspectionConfigurationName =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- You must specify the ARN or the name, and you can specify both.
deleteTLSInspectionConfiguration_tLSInspectionConfigurationArn :: Lens.Lens' DeleteTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
deleteTLSInspectionConfiguration_tLSInspectionConfigurationArn = Lens.lens (\DeleteTLSInspectionConfiguration' {tLSInspectionConfigurationArn} -> tLSInspectionConfigurationArn) (\s@DeleteTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationArn = a} :: DeleteTLSInspectionConfiguration)

-- | The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
deleteTLSInspectionConfiguration_tLSInspectionConfigurationName :: Lens.Lens' DeleteTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
deleteTLSInspectionConfiguration_tLSInspectionConfigurationName = Lens.lens (\DeleteTLSInspectionConfiguration' {tLSInspectionConfigurationName} -> tLSInspectionConfigurationName) (\s@DeleteTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationName = a} :: DeleteTLSInspectionConfiguration)

instance
  Core.AWSRequest
    DeleteTLSInspectionConfiguration
  where
  type
    AWSResponse DeleteTLSInspectionConfiguration =
      DeleteTLSInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTLSInspectionConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TLSInspectionConfigurationResponse")
      )

instance
  Prelude.Hashable
    DeleteTLSInspectionConfiguration
  where
  hashWithSalt
    _salt
    DeleteTLSInspectionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` tLSInspectionConfigurationArn
        `Prelude.hashWithSalt` tLSInspectionConfigurationName

instance
  Prelude.NFData
    DeleteTLSInspectionConfiguration
  where
  rnf DeleteTLSInspectionConfiguration' {..} =
    Prelude.rnf tLSInspectionConfigurationArn
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationName

instance
  Data.ToHeaders
    DeleteTLSInspectionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DeleteTLSInspectionConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTLSInspectionConfiguration where
  toJSON DeleteTLSInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TLSInspectionConfigurationArn" Data..=)
              Prelude.<$> tLSInspectionConfigurationArn,
            ("TLSInspectionConfigurationName" Data..=)
              Prelude.<$> tLSInspectionConfigurationName
          ]
      )

instance Data.ToPath DeleteTLSInspectionConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteTLSInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTLSInspectionConfigurationResponse' smart constructor.
data DeleteTLSInspectionConfigurationResponse = DeleteTLSInspectionConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The high-level properties of a TLS inspection configuration. This, along
    -- with the TLSInspectionConfiguration, define the TLS inspection
    -- configuration. You can retrieve all objects for a TLS inspection
    -- configuration by calling DescribeTLSInspectionConfiguration.
    tLSInspectionConfigurationResponse :: TLSInspectionConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTLSInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTLSInspectionConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'tLSInspectionConfigurationResponse', 'deleteTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse' - The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
newDeleteTLSInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'tLSInspectionConfigurationResponse'
  TLSInspectionConfigurationResponse ->
  DeleteTLSInspectionConfigurationResponse
newDeleteTLSInspectionConfigurationResponse
  pHttpStatus_
  pTLSInspectionConfigurationResponse_ =
    DeleteTLSInspectionConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        tLSInspectionConfigurationResponse =
          pTLSInspectionConfigurationResponse_
      }

-- | The response's http status code.
deleteTLSInspectionConfigurationResponse_httpStatus :: Lens.Lens' DeleteTLSInspectionConfigurationResponse Prelude.Int
deleteTLSInspectionConfigurationResponse_httpStatus = Lens.lens (\DeleteTLSInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteTLSInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteTLSInspectionConfigurationResponse)

-- | The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
deleteTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse :: Lens.Lens' DeleteTLSInspectionConfigurationResponse TLSInspectionConfigurationResponse
deleteTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse = Lens.lens (\DeleteTLSInspectionConfigurationResponse' {tLSInspectionConfigurationResponse} -> tLSInspectionConfigurationResponse) (\s@DeleteTLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationResponse = a} :: DeleteTLSInspectionConfigurationResponse)

instance
  Prelude.NFData
    DeleteTLSInspectionConfigurationResponse
  where
  rnf DeleteTLSInspectionConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationResponse
