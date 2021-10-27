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
-- Module      : Network.AWS.RAM.EnableSharingWithAwsOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables resource sharing within your organization in Organizations.
--
-- The caller must be the master account for the organization.
module Network.AWS.RAM.EnableSharingWithAwsOrganization
  ( -- * Creating a Request
    EnableSharingWithAwsOrganization (..),
    newEnableSharingWithAwsOrganization,

    -- * Destructuring the Response
    EnableSharingWithAwsOrganizationResponse (..),
    newEnableSharingWithAwsOrganizationResponse,

    -- * Response Lenses
    enableSharingWithAwsOrganizationResponse_returnValue,
    enableSharingWithAwsOrganizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableSharingWithAwsOrganization' smart constructor.
data EnableSharingWithAwsOrganization = EnableSharingWithAwsOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSharingWithAwsOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableSharingWithAwsOrganization ::
  EnableSharingWithAwsOrganization
newEnableSharingWithAwsOrganization =
  EnableSharingWithAwsOrganization'

instance
  Core.AWSRequest
    EnableSharingWithAwsOrganization
  where
  type
    AWSResponse EnableSharingWithAwsOrganization =
      EnableSharingWithAwsOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableSharingWithAwsOrganizationResponse'
            Prelude.<$> (x Core..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableSharingWithAwsOrganization

instance
  Prelude.NFData
    EnableSharingWithAwsOrganization

instance
  Core.ToHeaders
    EnableSharingWithAwsOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON EnableSharingWithAwsOrganization where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath EnableSharingWithAwsOrganization where
  toPath =
    Prelude.const "/enablesharingwithawsorganization"

instance
  Core.ToQuery
    EnableSharingWithAwsOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableSharingWithAwsOrganizationResponse' smart constructor.
data EnableSharingWithAwsOrganizationResponse = EnableSharingWithAwsOrganizationResponse'
  { -- | Indicates whether the request succeeded.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSharingWithAwsOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'enableSharingWithAwsOrganizationResponse_returnValue' - Indicates whether the request succeeded.
--
-- 'httpStatus', 'enableSharingWithAwsOrganizationResponse_httpStatus' - The response's http status code.
newEnableSharingWithAwsOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSharingWithAwsOrganizationResponse
newEnableSharingWithAwsOrganizationResponse
  pHttpStatus_ =
    EnableSharingWithAwsOrganizationResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates whether the request succeeded.
enableSharingWithAwsOrganizationResponse_returnValue :: Lens.Lens' EnableSharingWithAwsOrganizationResponse (Prelude.Maybe Prelude.Bool)
enableSharingWithAwsOrganizationResponse_returnValue = Lens.lens (\EnableSharingWithAwsOrganizationResponse' {returnValue} -> returnValue) (\s@EnableSharingWithAwsOrganizationResponse' {} a -> s {returnValue = a} :: EnableSharingWithAwsOrganizationResponse)

-- | The response's http status code.
enableSharingWithAwsOrganizationResponse_httpStatus :: Lens.Lens' EnableSharingWithAwsOrganizationResponse Prelude.Int
enableSharingWithAwsOrganizationResponse_httpStatus = Lens.lens (\EnableSharingWithAwsOrganizationResponse' {httpStatus} -> httpStatus) (\s@EnableSharingWithAwsOrganizationResponse' {} a -> s {httpStatus = a} :: EnableSharingWithAwsOrganizationResponse)

instance
  Prelude.NFData
    EnableSharingWithAwsOrganizationResponse
