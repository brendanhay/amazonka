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
-- Module      : Network.AWS.Organizations.DeleteOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the organization. You can delete an organization only by using
-- credentials from the management account. The organization must be empty
-- of member accounts.
module Network.AWS.Organizations.DeleteOrganization
  ( -- * Creating a Request
    DeleteOrganization (..),
    newDeleteOrganization,

    -- * Destructuring the Response
    DeleteOrganizationResponse (..),
    newDeleteOrganizationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOrganization ::
  DeleteOrganization
newDeleteOrganization = DeleteOrganization'

instance Prelude.AWSRequest DeleteOrganization where
  type
    Rs DeleteOrganization =
      DeleteOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteOrganizationResponse'

instance Prelude.Hashable DeleteOrganization

instance Prelude.NFData DeleteOrganization

instance Prelude.ToHeaders DeleteOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.DeleteOrganization" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteOrganization where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DeleteOrganization where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOrganizationResponse ::
  DeleteOrganizationResponse
newDeleteOrganizationResponse =
  DeleteOrganizationResponse'

instance Prelude.NFData DeleteOrganizationResponse
