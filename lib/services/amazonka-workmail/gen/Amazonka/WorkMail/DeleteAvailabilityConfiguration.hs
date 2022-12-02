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
-- Module      : Amazonka.WorkMail.DeleteAvailabilityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @AvailabilityConfiguration@ for the given WorkMail
-- organization and domain.
module Amazonka.WorkMail.DeleteAvailabilityConfiguration
  ( -- * Creating a Request
    DeleteAvailabilityConfiguration (..),
    newDeleteAvailabilityConfiguration,

    -- * Request Lenses
    deleteAvailabilityConfiguration_organizationId,
    deleteAvailabilityConfiguration_domainName,

    -- * Destructuring the Response
    DeleteAvailabilityConfigurationResponse (..),
    newDeleteAvailabilityConfigurationResponse,

    -- * Response Lenses
    deleteAvailabilityConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteAvailabilityConfiguration' smart constructor.
data DeleteAvailabilityConfiguration = DeleteAvailabilityConfiguration'
  { -- | The WorkMail organization for which the @AvailabilityConfiguration@ will
    -- be deleted.
    organizationId :: Prelude.Text,
    -- | The domain for which the @AvailabilityConfiguration@ will be deleted.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAvailabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteAvailabilityConfiguration_organizationId' - The WorkMail organization for which the @AvailabilityConfiguration@ will
-- be deleted.
--
-- 'domainName', 'deleteAvailabilityConfiguration_domainName' - The domain for which the @AvailabilityConfiguration@ will be deleted.
newDeleteAvailabilityConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DeleteAvailabilityConfiguration
newDeleteAvailabilityConfiguration
  pOrganizationId_
  pDomainName_ =
    DeleteAvailabilityConfiguration'
      { organizationId =
          pOrganizationId_,
        domainName = pDomainName_
      }

-- | The WorkMail organization for which the @AvailabilityConfiguration@ will
-- be deleted.
deleteAvailabilityConfiguration_organizationId :: Lens.Lens' DeleteAvailabilityConfiguration Prelude.Text
deleteAvailabilityConfiguration_organizationId = Lens.lens (\DeleteAvailabilityConfiguration' {organizationId} -> organizationId) (\s@DeleteAvailabilityConfiguration' {} a -> s {organizationId = a} :: DeleteAvailabilityConfiguration)

-- | The domain for which the @AvailabilityConfiguration@ will be deleted.
deleteAvailabilityConfiguration_domainName :: Lens.Lens' DeleteAvailabilityConfiguration Prelude.Text
deleteAvailabilityConfiguration_domainName = Lens.lens (\DeleteAvailabilityConfiguration' {domainName} -> domainName) (\s@DeleteAvailabilityConfiguration' {} a -> s {domainName = a} :: DeleteAvailabilityConfiguration)

instance
  Core.AWSRequest
    DeleteAvailabilityConfiguration
  where
  type
    AWSResponse DeleteAvailabilityConfiguration =
      DeleteAvailabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAvailabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAvailabilityConfiguration
  where
  hashWithSalt
    _salt
    DeleteAvailabilityConfiguration' {..} =
      _salt `Prelude.hashWithSalt` organizationId
        `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    DeleteAvailabilityConfiguration
  where
  rnf DeleteAvailabilityConfiguration' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance
  Data.ToHeaders
    DeleteAvailabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteAvailabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAvailabilityConfiguration where
  toJSON DeleteAvailabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath DeleteAvailabilityConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAvailabilityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAvailabilityConfigurationResponse' smart constructor.
data DeleteAvailabilityConfigurationResponse = DeleteAvailabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAvailabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAvailabilityConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAvailabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAvailabilityConfigurationResponse
newDeleteAvailabilityConfigurationResponse
  pHttpStatus_ =
    DeleteAvailabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAvailabilityConfigurationResponse_httpStatus :: Lens.Lens' DeleteAvailabilityConfigurationResponse Prelude.Int
deleteAvailabilityConfigurationResponse_httpStatus = Lens.lens (\DeleteAvailabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAvailabilityConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAvailabilityConfigurationResponse)

instance
  Prelude.NFData
    DeleteAvailabilityConfigurationResponse
  where
  rnf DeleteAvailabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus
