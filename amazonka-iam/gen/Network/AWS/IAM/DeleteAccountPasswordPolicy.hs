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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAccountPasswordPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountPasswordPolicy ::
  DeleteAccountPasswordPolicy
newDeleteAccountPasswordPolicy =
  DeleteAccountPasswordPolicy'

instance Core.AWSRequest DeleteAccountPasswordPolicy where
  type
    AWSResponse DeleteAccountPasswordPolicy =
      DeleteAccountPasswordPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteAccountPasswordPolicyResponse'

instance Core.Hashable DeleteAccountPasswordPolicy

instance Core.NFData DeleteAccountPasswordPolicy

instance Core.ToHeaders DeleteAccountPasswordPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteAccountPasswordPolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAccountPasswordPolicy where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DeleteAccountPasswordPolicy" :: Core.ByteString),
            "Version" Core.=: ("2010-05-08" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDeleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAccountPasswordPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountPasswordPolicyResponse ::
  DeleteAccountPasswordPolicyResponse
newDeleteAccountPasswordPolicyResponse =
  DeleteAccountPasswordPolicyResponse'

instance
  Core.NFData
    DeleteAccountPasswordPolicyResponse
