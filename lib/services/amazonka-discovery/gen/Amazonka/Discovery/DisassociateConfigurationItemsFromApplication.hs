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
-- Module      : Amazonka.Discovery.DisassociateConfigurationItemsFromApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates one or more configuration items from an application.
module Amazonka.Discovery.DisassociateConfigurationItemsFromApplication
  ( -- * Creating a Request
    DisassociateConfigurationItemsFromApplication (..),
    newDisassociateConfigurationItemsFromApplication,

    -- * Request Lenses
    disassociateConfigurationItemsFromApplication_applicationConfigurationId,
    disassociateConfigurationItemsFromApplication_configurationIds,

    -- * Destructuring the Response
    DisassociateConfigurationItemsFromApplicationResponse (..),
    newDisassociateConfigurationItemsFromApplicationResponse,

    -- * Response Lenses
    disassociateConfigurationItemsFromApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateConfigurationItemsFromApplication' smart constructor.
data DisassociateConfigurationItemsFromApplication = DisassociateConfigurationItemsFromApplication'
  { -- | Configuration ID of an application from which each item is
    -- disassociated.
    applicationConfigurationId :: Prelude.Text,
    -- | Configuration ID of each item to be disassociated from an application.
    configurationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConfigurationItemsFromApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationConfigurationId', 'disassociateConfigurationItemsFromApplication_applicationConfigurationId' - Configuration ID of an application from which each item is
-- disassociated.
--
-- 'configurationIds', 'disassociateConfigurationItemsFromApplication_configurationIds' - Configuration ID of each item to be disassociated from an application.
newDisassociateConfigurationItemsFromApplication ::
  -- | 'applicationConfigurationId'
  Prelude.Text ->
  DisassociateConfigurationItemsFromApplication
newDisassociateConfigurationItemsFromApplication
  pApplicationConfigurationId_ =
    DisassociateConfigurationItemsFromApplication'
      { applicationConfigurationId =
          pApplicationConfigurationId_,
        configurationIds =
          Prelude.mempty
      }

-- | Configuration ID of an application from which each item is
-- disassociated.
disassociateConfigurationItemsFromApplication_applicationConfigurationId :: Lens.Lens' DisassociateConfigurationItemsFromApplication Prelude.Text
disassociateConfigurationItemsFromApplication_applicationConfigurationId = Lens.lens (\DisassociateConfigurationItemsFromApplication' {applicationConfigurationId} -> applicationConfigurationId) (\s@DisassociateConfigurationItemsFromApplication' {} a -> s {applicationConfigurationId = a} :: DisassociateConfigurationItemsFromApplication)

-- | Configuration ID of each item to be disassociated from an application.
disassociateConfigurationItemsFromApplication_configurationIds :: Lens.Lens' DisassociateConfigurationItemsFromApplication [Prelude.Text]
disassociateConfigurationItemsFromApplication_configurationIds = Lens.lens (\DisassociateConfigurationItemsFromApplication' {configurationIds} -> configurationIds) (\s@DisassociateConfigurationItemsFromApplication' {} a -> s {configurationIds = a} :: DisassociateConfigurationItemsFromApplication) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociateConfigurationItemsFromApplication
  where
  type
    AWSResponse
      DisassociateConfigurationItemsFromApplication =
      DisassociateConfigurationItemsFromApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConfigurationItemsFromApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateConfigurationItemsFromApplication
  where
  hashWithSalt
    _salt
    DisassociateConfigurationItemsFromApplication' {..} =
      _salt
        `Prelude.hashWithSalt` applicationConfigurationId
        `Prelude.hashWithSalt` configurationIds

instance
  Prelude.NFData
    DisassociateConfigurationItemsFromApplication
  where
  rnf
    DisassociateConfigurationItemsFromApplication' {..} =
      Prelude.rnf applicationConfigurationId `Prelude.seq`
        Prelude.rnf configurationIds

instance
  Data.ToHeaders
    DisassociateConfigurationItemsFromApplication
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.DisassociateConfigurationItemsFromApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateConfigurationItemsFromApplication
  where
  toJSON
    DisassociateConfigurationItemsFromApplication' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "applicationConfigurationId"
                    Data..= applicationConfigurationId
                ),
              Prelude.Just
                ("configurationIds" Data..= configurationIds)
            ]
        )

instance
  Data.ToPath
    DisassociateConfigurationItemsFromApplication
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateConfigurationItemsFromApplication
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateConfigurationItemsFromApplicationResponse' smart constructor.
data DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConfigurationItemsFromApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateConfigurationItemsFromApplicationResponse_httpStatus' - The response's http status code.
newDisassociateConfigurationItemsFromApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateConfigurationItemsFromApplicationResponse
newDisassociateConfigurationItemsFromApplicationResponse
  pHttpStatus_ =
    DisassociateConfigurationItemsFromApplicationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateConfigurationItemsFromApplicationResponse_httpStatus :: Lens.Lens' DisassociateConfigurationItemsFromApplicationResponse Prelude.Int
disassociateConfigurationItemsFromApplicationResponse_httpStatus = Lens.lens (\DisassociateConfigurationItemsFromApplicationResponse' {httpStatus} -> httpStatus) (\s@DisassociateConfigurationItemsFromApplicationResponse' {} a -> s {httpStatus = a} :: DisassociateConfigurationItemsFromApplicationResponse)

instance
  Prelude.NFData
    DisassociateConfigurationItemsFromApplicationResponse
  where
  rnf
    DisassociateConfigurationItemsFromApplicationResponse' {..} =
      Prelude.rnf httpStatus
