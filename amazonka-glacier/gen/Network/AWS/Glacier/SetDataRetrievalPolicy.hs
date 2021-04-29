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
-- Module      : Network.AWS.Glacier.SetDataRetrievalPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets and then enacts a data retrieval policy in the
-- region specified in the PUT request. You can set one policy per region
-- for an AWS account. The policy is enacted within a few minutes of a
-- successful PUT operation.
--
-- The set policy operation does not affect retrieval jobs that were in
-- progress before the policy was enacted. For more information about data
-- retrieval policies, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies>.
module Network.AWS.Glacier.SetDataRetrievalPolicy
  ( -- * Creating a Request
    SetDataRetrievalPolicy (..),
    newSetDataRetrievalPolicy,

    -- * Request Lenses
    setDataRetrievalPolicy_policy,
    setDataRetrievalPolicy_accountId,

    -- * Destructuring the Response
    SetDataRetrievalPolicyResponse (..),
    newSetDataRetrievalPolicyResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | SetDataRetrievalPolicy input.
--
-- /See:/ 'newSetDataRetrievalPolicy' smart constructor.
data SetDataRetrievalPolicy = SetDataRetrievalPolicy'
  { -- | The data retrieval policy in JSON format.
    policy :: Prelude.Maybe DataRetrievalPolicy,
    -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetDataRetrievalPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'setDataRetrievalPolicy_policy' - The data retrieval policy in JSON format.
--
-- 'accountId', 'setDataRetrievalPolicy_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
newSetDataRetrievalPolicy ::
  -- | 'accountId'
  Prelude.Text ->
  SetDataRetrievalPolicy
newSetDataRetrievalPolicy pAccountId_ =
  SetDataRetrievalPolicy'
    { policy = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The data retrieval policy in JSON format.
setDataRetrievalPolicy_policy :: Lens.Lens' SetDataRetrievalPolicy (Prelude.Maybe DataRetrievalPolicy)
setDataRetrievalPolicy_policy = Lens.lens (\SetDataRetrievalPolicy' {policy} -> policy) (\s@SetDataRetrievalPolicy' {} a -> s {policy = a} :: SetDataRetrievalPolicy)

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
setDataRetrievalPolicy_accountId :: Lens.Lens' SetDataRetrievalPolicy Prelude.Text
setDataRetrievalPolicy_accountId = Lens.lens (\SetDataRetrievalPolicy' {accountId} -> accountId) (\s@SetDataRetrievalPolicy' {} a -> s {accountId = a} :: SetDataRetrievalPolicy)

instance Prelude.AWSRequest SetDataRetrievalPolicy where
  type
    Rs SetDataRetrievalPolicy =
      SetDataRetrievalPolicyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull
      SetDataRetrievalPolicyResponse'

instance Prelude.Hashable SetDataRetrievalPolicy

instance Prelude.NFData SetDataRetrievalPolicy

instance Prelude.ToHeaders SetDataRetrievalPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON SetDataRetrievalPolicy where
  toJSON SetDataRetrievalPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Policy" Prelude..=) Prelude.<$> policy]
      )

instance Prelude.ToPath SetDataRetrievalPolicy where
  toPath SetDataRetrievalPolicy' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/policies/data-retrieval"
      ]

instance Prelude.ToQuery SetDataRetrievalPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetDataRetrievalPolicyResponse' smart constructor.
data SetDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetDataRetrievalPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetDataRetrievalPolicyResponse ::
  SetDataRetrievalPolicyResponse
newSetDataRetrievalPolicyResponse =
  SetDataRetrievalPolicyResponse'

instance
  Prelude.NFData
    SetDataRetrievalPolicyResponse
