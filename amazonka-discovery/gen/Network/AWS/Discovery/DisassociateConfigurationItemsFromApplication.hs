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

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateConfigurationItemsFromApplication' smart constructor.
data DisassociateConfigurationItemsFromApplication = DisassociateConfigurationItemsFromApplication'
  { -- | Configuration ID of an application from which each item is
    -- disassociated.
    applicationConfigurationId :: Core.Text,
    -- | Configuration ID of each item to be disassociated from an application.
    configurationIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DisassociateConfigurationItemsFromApplication
newDisassociateConfigurationItemsFromApplication
  pApplicationConfigurationId_ =
    DisassociateConfigurationItemsFromApplication'
      { applicationConfigurationId =
          pApplicationConfigurationId_,
        configurationIds =
          Core.mempty
      }

-- | Configuration ID of an application from which each item is
-- disassociated.
disassociateConfigurationItemsFromApplication_applicationConfigurationId :: Lens.Lens' DisassociateConfigurationItemsFromApplication Core.Text
disassociateConfigurationItemsFromApplication_applicationConfigurationId = Lens.lens (\DisassociateConfigurationItemsFromApplication' {applicationConfigurationId} -> applicationConfigurationId) (\s@DisassociateConfigurationItemsFromApplication' {} a -> s {applicationConfigurationId = a} :: DisassociateConfigurationItemsFromApplication)

-- | Configuration ID of each item to be disassociated from an application.
disassociateConfigurationItemsFromApplication_configurationIds :: Lens.Lens' DisassociateConfigurationItemsFromApplication [Core.Text]
disassociateConfigurationItemsFromApplication_configurationIds = Lens.lens (\DisassociateConfigurationItemsFromApplication' {configurationIds} -> configurationIds) (\s@DisassociateConfigurationItemsFromApplication' {} a -> s {configurationIds = a} :: DisassociateConfigurationItemsFromApplication) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DisassociateConfigurationItemsFromApplication
  where
  type
    AWSResponse
      DisassociateConfigurationItemsFromApplication =
      DisassociateConfigurationItemsFromApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConfigurationItemsFromApplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateConfigurationItemsFromApplication

instance
  Core.NFData
    DisassociateConfigurationItemsFromApplication

instance
  Core.ToHeaders
    DisassociateConfigurationItemsFromApplication
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DisassociateConfigurationItemsFromApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DisassociateConfigurationItemsFromApplication
  where
  toJSON
    DisassociateConfigurationItemsFromApplication' {..} =
      Core.object
        ( Core.catMaybes
            [ Core.Just
                ( "applicationConfigurationId"
                    Core..= applicationConfigurationId
                ),
              Core.Just
                ("configurationIds" Core..= configurationIds)
            ]
        )

instance
  Core.ToPath
    DisassociateConfigurationItemsFromApplication
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateConfigurationItemsFromApplication
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateConfigurationItemsFromApplicationResponse' smart constructor.
data DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateConfigurationItemsFromApplicationResponse
newDisassociateConfigurationItemsFromApplicationResponse
  pHttpStatus_ =
    DisassociateConfigurationItemsFromApplicationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateConfigurationItemsFromApplicationResponse_httpStatus :: Lens.Lens' DisassociateConfigurationItemsFromApplicationResponse Core.Int
disassociateConfigurationItemsFromApplicationResponse_httpStatus = Lens.lens (\DisassociateConfigurationItemsFromApplicationResponse' {httpStatus} -> httpStatus) (\s@DisassociateConfigurationItemsFromApplicationResponse' {} a -> s {httpStatus = a} :: DisassociateConfigurationItemsFromApplicationResponse)

instance
  Core.NFData
    DisassociateConfigurationItemsFromApplicationResponse
