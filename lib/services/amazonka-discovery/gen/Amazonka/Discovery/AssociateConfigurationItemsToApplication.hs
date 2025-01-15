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
-- Module      : Amazonka.Discovery.AssociateConfigurationItemsToApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more configuration items with an application.
module Amazonka.Discovery.AssociateConfigurationItemsToApplication
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
associateConfigurationItemsToApplication_configurationIds = Lens.lens (\AssociateConfigurationItemsToApplication' {configurationIds} -> configurationIds) (\s@AssociateConfigurationItemsToApplication' {} a -> s {configurationIds = a} :: AssociateConfigurationItemsToApplication) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AssociateConfigurationItemsToApplication
  where
  type
    AWSResponse
      AssociateConfigurationItemsToApplication =
      AssociateConfigurationItemsToApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateConfigurationItemsToApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateConfigurationItemsToApplication
  where
  hashWithSalt
    _salt
    AssociateConfigurationItemsToApplication' {..} =
      _salt
        `Prelude.hashWithSalt` applicationConfigurationId
        `Prelude.hashWithSalt` configurationIds

instance
  Prelude.NFData
    AssociateConfigurationItemsToApplication
  where
  rnf AssociateConfigurationItemsToApplication' {..} =
    Prelude.rnf applicationConfigurationId `Prelude.seq`
      Prelude.rnf configurationIds

instance
  Data.ToHeaders
    AssociateConfigurationItemsToApplication
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.AssociateConfigurationItemsToApplication" ::
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
    AssociateConfigurationItemsToApplication
  where
  toJSON AssociateConfigurationItemsToApplication' {..} =
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
    AssociateConfigurationItemsToApplication
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
  where
  rnf
    AssociateConfigurationItemsToApplicationResponse' {..} =
      Prelude.rnf httpStatus
