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
-- Module      : Network.AWS.Discovery.AssociateConfigurationItemsToApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more configuration items with an application.
module Network.AWS.Discovery.AssociateConfigurationItemsToApplication
  ( -- * Creating a Request
    AssociateConfigurationItemsToApplication (..),
    newAssociateConfigurationItemsToApplication,

    -- * Request Lenses
    associateConfigurationItemsToApplication_applicationConfigurationId,
    associateConfigurationItemsToApplication_configurationIds,

    -- * Destructuring the Response
    AssociateConfigurationItemsToApplicationResponse (..),
    newAssociateConfigurationItemsToApplicationResponse,

    -- * Response Lenses
    associateConfigurationItemsToApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateConfigurationItemsToApplication' smart constructor.
data AssociateConfigurationItemsToApplication = AssociateConfigurationItemsToApplication'
  { -- | The configuration ID of an application with which items are to be
    -- associated.
    applicationConfigurationId :: Prelude.Text,
    -- | The ID of each configuration item to be associated with an application.
    configurationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateConfigurationItemsToApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationConfigurationId', 'associateConfigurationItemsToApplication_applicationConfigurationId' - The configuration ID of an application with which items are to be
-- associated.
--
-- 'configurationIds', 'associateConfigurationItemsToApplication_configurationIds' - The ID of each configuration item to be associated with an application.
newAssociateConfigurationItemsToApplication ::
  -- | 'applicationConfigurationId'
  Prelude.Text ->
  AssociateConfigurationItemsToApplication
newAssociateConfigurationItemsToApplication
  pApplicationConfigurationId_ =
    AssociateConfigurationItemsToApplication'
      { applicationConfigurationId =
          pApplicationConfigurationId_,
        configurationIds = Prelude.mempty
      }

-- | The configuration ID of an application with which items are to be
-- associated.
associateConfigurationItemsToApplication_applicationConfigurationId :: Lens.Lens' AssociateConfigurationItemsToApplication Prelude.Text
associateConfigurationItemsToApplication_applicationConfigurationId = Lens.lens (\AssociateConfigurationItemsToApplication' {applicationConfigurationId} -> applicationConfigurationId) (\s@AssociateConfigurationItemsToApplication' {} a -> s {applicationConfigurationId = a} :: AssociateConfigurationItemsToApplication)

-- | The ID of each configuration item to be associated with an application.
associateConfigurationItemsToApplication_configurationIds :: Lens.Lens' AssociateConfigurationItemsToApplication [Prelude.Text]
associateConfigurationItemsToApplication_configurationIds = Lens.lens (\AssociateConfigurationItemsToApplication' {configurationIds} -> configurationIds) (\s@AssociateConfigurationItemsToApplication' {} a -> s {configurationIds = a} :: AssociateConfigurationItemsToApplication) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    AssociateConfigurationItemsToApplication
  where
  type
    AWSResponse
      AssociateConfigurationItemsToApplication =
      AssociateConfigurationItemsToApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateConfigurationItemsToApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateConfigurationItemsToApplication

instance
  Prelude.NFData
    AssociateConfigurationItemsToApplication

instance
  Core.ToHeaders
    AssociateConfigurationItemsToApplication
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.AssociateConfigurationItemsToApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    AssociateConfigurationItemsToApplication
  where
  toJSON AssociateConfigurationItemsToApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "applicationConfigurationId"
                  Core..= applicationConfigurationId
              ),
            Prelude.Just
              ("configurationIds" Core..= configurationIds)
          ]
      )

instance
  Core.ToPath
    AssociateConfigurationItemsToApplication
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AssociateConfigurationItemsToApplication
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateConfigurationItemsToApplicationResponse' smart constructor.
data AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateConfigurationItemsToApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateConfigurationItemsToApplicationResponse_httpStatus' - The response's http status code.
newAssociateConfigurationItemsToApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateConfigurationItemsToApplicationResponse
newAssociateConfigurationItemsToApplicationResponse
  pHttpStatus_ =
    AssociateConfigurationItemsToApplicationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateConfigurationItemsToApplicationResponse_httpStatus :: Lens.Lens' AssociateConfigurationItemsToApplicationResponse Prelude.Int
associateConfigurationItemsToApplicationResponse_httpStatus = Lens.lens (\AssociateConfigurationItemsToApplicationResponse' {httpStatus} -> httpStatus) (\s@AssociateConfigurationItemsToApplicationResponse' {} a -> s {httpStatus = a} :: AssociateConfigurationItemsToApplicationResponse)

instance
  Prelude.NFData
    AssociateConfigurationItemsToApplicationResponse
