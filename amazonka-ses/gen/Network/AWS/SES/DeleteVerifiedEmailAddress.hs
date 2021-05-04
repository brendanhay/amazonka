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
-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @DeleteIdentity@ operation to delete email addresses
-- and domains.
module Network.AWS.SES.DeleteVerifiedEmailAddress
  ( -- * Creating a Request
    DeleteVerifiedEmailAddress (..),
    newDeleteVerifiedEmailAddress,

    -- * Request Lenses
    deleteVerifiedEmailAddress_emailAddress,

    -- * Destructuring the Response
    DeleteVerifiedEmailAddressResponse (..),
    newDeleteVerifiedEmailAddressResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to delete an email address from the list of email
-- addresses you have attempted to verify under your AWS account.
--
-- /See:/ 'newDeleteVerifiedEmailAddress' smart constructor.
data DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress'
  { -- | An email address to be removed from the list of verified addresses.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedEmailAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'deleteVerifiedEmailAddress_emailAddress' - An email address to be removed from the list of verified addresses.
newDeleteVerifiedEmailAddress ::
  -- | 'emailAddress'
  Prelude.Text ->
  DeleteVerifiedEmailAddress
newDeleteVerifiedEmailAddress pEmailAddress_ =
  DeleteVerifiedEmailAddress'
    { emailAddress =
        pEmailAddress_
    }

-- | An email address to be removed from the list of verified addresses.
deleteVerifiedEmailAddress_emailAddress :: Lens.Lens' DeleteVerifiedEmailAddress Prelude.Text
deleteVerifiedEmailAddress_emailAddress = Lens.lens (\DeleteVerifiedEmailAddress' {emailAddress} -> emailAddress) (\s@DeleteVerifiedEmailAddress' {} a -> s {emailAddress = a} :: DeleteVerifiedEmailAddress)

instance
  Prelude.AWSRequest
    DeleteVerifiedEmailAddress
  where
  type
    Rs DeleteVerifiedEmailAddress =
      DeleteVerifiedEmailAddressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteVerifiedEmailAddressResponse'

instance Prelude.Hashable DeleteVerifiedEmailAddress

instance Prelude.NFData DeleteVerifiedEmailAddress

instance Prelude.ToHeaders DeleteVerifiedEmailAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVerifiedEmailAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVerifiedEmailAddress where
  toQuery DeleteVerifiedEmailAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteVerifiedEmailAddress" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EmailAddress" Prelude.=: emailAddress
      ]

-- | /See:/ 'newDeleteVerifiedEmailAddressResponse' smart constructor.
data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVerifiedEmailAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVerifiedEmailAddressResponse ::
  DeleteVerifiedEmailAddressResponse
newDeleteVerifiedEmailAddressResponse =
  DeleteVerifiedEmailAddressResponse'

instance
  Prelude.NFData
    DeleteVerifiedEmailAddressResponse
