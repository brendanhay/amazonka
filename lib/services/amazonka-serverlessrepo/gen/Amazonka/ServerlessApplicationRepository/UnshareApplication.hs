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
-- Module      : Amazonka.ServerlessApplicationRepository.UnshareApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unshares an application from an AWS Organization.
--
-- This operation can be called only from the organization\'s master
-- account.
module Amazonka.ServerlessApplicationRepository.UnshareApplication
  ( -- * Creating a Request
    UnshareApplication (..),
    newUnshareApplication,

    -- * Request Lenses
    unshareApplication_applicationId,
    unshareApplication_organizationId,

    -- * Destructuring the Response
    UnshareApplicationResponse (..),
    newUnshareApplicationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newUnshareApplication' smart constructor.
data UnshareApplication = UnshareApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text,
    -- | The AWS Organization ID to unshare the application from.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnshareApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'unshareApplication_applicationId' - The Amazon Resource Name (ARN) of the application.
--
-- 'organizationId', 'unshareApplication_organizationId' - The AWS Organization ID to unshare the application from.
newUnshareApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'organizationId'
  Prelude.Text ->
  UnshareApplication
newUnshareApplication
  pApplicationId_
  pOrganizationId_ =
    UnshareApplication'
      { applicationId =
          pApplicationId_,
        organizationId = pOrganizationId_
      }

-- | The Amazon Resource Name (ARN) of the application.
unshareApplication_applicationId :: Lens.Lens' UnshareApplication Prelude.Text
unshareApplication_applicationId = Lens.lens (\UnshareApplication' {applicationId} -> applicationId) (\s@UnshareApplication' {} a -> s {applicationId = a} :: UnshareApplication)

-- | The AWS Organization ID to unshare the application from.
unshareApplication_organizationId :: Lens.Lens' UnshareApplication Prelude.Text
unshareApplication_organizationId = Lens.lens (\UnshareApplication' {organizationId} -> organizationId) (\s@UnshareApplication' {} a -> s {organizationId = a} :: UnshareApplication)

instance Core.AWSRequest UnshareApplication where
  type
    AWSResponse UnshareApplication =
      UnshareApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UnshareApplicationResponse'

instance Prelude.Hashable UnshareApplication where
  hashWithSalt _salt UnshareApplication' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData UnshareApplication where
  rnf UnshareApplication' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf organizationId

instance Data.ToHeaders UnshareApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnshareApplication where
  toJSON UnshareApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("organizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath UnshareApplication where
  toPath UnshareApplication' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/unshare"
      ]

instance Data.ToQuery UnshareApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnshareApplicationResponse' smart constructor.
data UnshareApplicationResponse = UnshareApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnshareApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnshareApplicationResponse ::
  UnshareApplicationResponse
newUnshareApplicationResponse =
  UnshareApplicationResponse'

instance Prelude.NFData UnshareApplicationResponse where
  rnf _ = ()
