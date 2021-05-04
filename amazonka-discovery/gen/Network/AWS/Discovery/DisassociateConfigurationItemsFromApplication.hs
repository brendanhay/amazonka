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
-- Module      : Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates one or more configuration items from an application.
module Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
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

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateConfigurationItemsFromApplication' smart constructor.
data DisassociateConfigurationItemsFromApplication = DisassociateConfigurationItemsFromApplication'
  { -- | Configuration ID of an application from which each item is
    -- disassociated.
    applicationConfigurationId :: Prelude.Text,
    -- | Configuration ID of each item to be disassociated from an application.
    configurationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
disassociateConfigurationItemsFromApplication_configurationIds = Lens.lens (\DisassociateConfigurationItemsFromApplication' {configurationIds} -> configurationIds) (\s@DisassociateConfigurationItemsFromApplication' {} a -> s {configurationIds = a} :: DisassociateConfigurationItemsFromApplication) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    DisassociateConfigurationItemsFromApplication
  where
  type
    Rs DisassociateConfigurationItemsFromApplication =
      DisassociateConfigurationItemsFromApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConfigurationItemsFromApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateConfigurationItemsFromApplication

instance
  Prelude.NFData
    DisassociateConfigurationItemsFromApplication

instance
  Prelude.ToHeaders
    DisassociateConfigurationItemsFromApplication
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSPoseidonService_V2015_11_01.DisassociateConfigurationItemsFromApplication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisassociateConfigurationItemsFromApplication
  where
  toJSON
    DisassociateConfigurationItemsFromApplication' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "applicationConfigurationId"
                    Prelude..= applicationConfigurationId
                ),
              Prelude.Just
                ("configurationIds" Prelude..= configurationIds)
            ]
        )

instance
  Prelude.ToPath
    DisassociateConfigurationItemsFromApplication
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateConfigurationItemsFromApplication
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateConfigurationItemsFromApplicationResponse' smart constructor.
data DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
