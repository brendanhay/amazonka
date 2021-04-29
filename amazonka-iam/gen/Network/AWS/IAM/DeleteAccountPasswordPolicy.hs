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
-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the AWS account. There are no
-- parameters.
module Network.AWS.IAM.DeleteAccountPasswordPolicy
  ( -- * Creating a Request
    DeleteAccountPasswordPolicy (..),
    newDeleteAccountPasswordPolicy,

    -- * Destructuring the Response
    DeleteAccountPasswordPolicyResponse (..),
    newDeleteAccountPasswordPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountPasswordPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountPasswordPolicy ::
  DeleteAccountPasswordPolicy
newDeleteAccountPasswordPolicy =
  DeleteAccountPasswordPolicy'

instance
  Prelude.AWSRequest
    DeleteAccountPasswordPolicy
  where
  type
    Rs DeleteAccountPasswordPolicy =
      DeleteAccountPasswordPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteAccountPasswordPolicyResponse'

instance Prelude.Hashable DeleteAccountPasswordPolicy

instance Prelude.NFData DeleteAccountPasswordPolicy

instance
  Prelude.ToHeaders
    DeleteAccountPasswordPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAccountPasswordPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAccountPasswordPolicy where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ( "DeleteAccountPasswordPolicy" ::
                             Prelude.ByteString
                         ),
            "Version"
              Prelude.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDeleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountPasswordPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountPasswordPolicyResponse ::
  DeleteAccountPasswordPolicyResponse
newDeleteAccountPasswordPolicyResponse =
  DeleteAccountPasswordPolicyResponse'

instance
  Prelude.NFData
    DeleteAccountPasswordPolicyResponse
