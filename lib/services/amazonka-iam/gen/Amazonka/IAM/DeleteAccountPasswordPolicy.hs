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
-- Module      : Amazonka.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the Amazon Web Services account. There
-- are no parameters.
module Amazonka.IAM.DeleteAccountPasswordPolicy
  ( -- * Creating a Request
    DeleteAccountPasswordPolicy (..),
    newDeleteAccountPasswordPolicy,

    -- * Destructuring the Response
    DeleteAccountPasswordPolicyResponse (..),
    newDeleteAccountPasswordPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAccountPasswordPolicyResponse'

instance Prelude.Hashable DeleteAccountPasswordPolicy where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteAccountPasswordPolicy where
  rnf _ = ()

instance Data.ToHeaders DeleteAccountPasswordPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAccountPasswordPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccountPasswordPolicy where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "DeleteAccountPasswordPolicy" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDeleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
