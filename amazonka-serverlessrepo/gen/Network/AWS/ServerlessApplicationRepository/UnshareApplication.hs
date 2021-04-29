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
-- Module      : Network.AWS.ServerlessApplicationRepository.UnshareApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unshares an application from an AWS Organization.
--
-- This operation can be called only from the organization\'s master
-- account.
module Network.AWS.ServerlessApplicationRepository.UnshareApplication
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newUnshareApplication' smart constructor.
data UnshareApplication = UnshareApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text,
    -- | The AWS Organization ID to unshare the application from.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UnshareApplication where
  type
    Rs UnshareApplication =
      UnshareApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UnshareApplicationResponse'

instance Prelude.Hashable UnshareApplication

instance Prelude.NFData UnshareApplication

instance Prelude.ToHeaders UnshareApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UnshareApplication where
  toJSON UnshareApplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("organizationId" Prelude..= organizationId)
          ]
      )

instance Prelude.ToPath UnshareApplication where
  toPath UnshareApplication' {..} =
    Prelude.mconcat
      [ "/applications/",
        Prelude.toBS applicationId,
        "/unshare"
      ]

instance Prelude.ToQuery UnshareApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnshareApplicationResponse' smart constructor.
data UnshareApplicationResponse = UnshareApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnshareApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnshareApplicationResponse ::
  UnshareApplicationResponse
newUnshareApplicationResponse =
  UnshareApplicationResponse'

instance Prelude.NFData UnshareApplicationResponse
