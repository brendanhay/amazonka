{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.SetDataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets and then enacts a data retrieval policy in the region specified in the PUT request. You can set one policy per region for an AWS account. The policy is enacted within a few minutes of a successful PUT operation.
--
-- The set policy operation does not affect retrieval jobs that were in progress before the policy was enacted. For more information about data retrieval policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies> .
module Network.AWS.Glacier.SetDataRetrievalPolicy
  ( -- * Creating a request
    SetDataRetrievalPolicy (..),
    mkSetDataRetrievalPolicy,

    -- ** Request lenses
    sdrpAccountId,
    sdrpPolicy,

    -- * Destructuring the response
    SetDataRetrievalPolicyResponse (..),
    mkSetDataRetrievalPolicyResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | SetDataRetrievalPolicy input.
--
-- /See:/ 'mkSetDataRetrievalPolicy' smart constructor.
data SetDataRetrievalPolicy = SetDataRetrievalPolicy'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text,
    -- | The data retrieval policy in JSON format.
    policy :: Lude.Maybe DataRetrievalPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDataRetrievalPolicy' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
-- * 'policy' - The data retrieval policy in JSON format.
mkSetDataRetrievalPolicy ::
  -- | 'accountId'
  Lude.Text ->
  SetDataRetrievalPolicy
mkSetDataRetrievalPolicy pAccountId_ =
  SetDataRetrievalPolicy'
    { accountId = pAccountId_,
      policy = Lude.Nothing
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrpAccountId :: Lens.Lens' SetDataRetrievalPolicy Lude.Text
sdrpAccountId = Lens.lens (accountId :: SetDataRetrievalPolicy -> Lude.Text) (\s a -> s {accountId = a} :: SetDataRetrievalPolicy)
{-# DEPRECATED sdrpAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The data retrieval policy in JSON format.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrpPolicy :: Lens.Lens' SetDataRetrievalPolicy (Lude.Maybe DataRetrievalPolicy)
sdrpPolicy = Lens.lens (policy :: SetDataRetrievalPolicy -> Lude.Maybe DataRetrievalPolicy) (\s a -> s {policy = a} :: SetDataRetrievalPolicy)
{-# DEPRECATED sdrpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest SetDataRetrievalPolicy where
  type Rs SetDataRetrievalPolicy = SetDataRetrievalPolicyResponse
  request = Req.putJSON glacierService
  response = Res.receiveNull SetDataRetrievalPolicyResponse'

instance Lude.ToHeaders SetDataRetrievalPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetDataRetrievalPolicy where
  toJSON SetDataRetrievalPolicy' {..} =
    Lude.object (Lude.catMaybes [("Policy" Lude..=) Lude.<$> policy])

instance Lude.ToPath SetDataRetrievalPolicy where
  toPath SetDataRetrievalPolicy' {..} =
    Lude.mconcat
      ["/", Lude.toBS accountId, "/policies/data-retrieval"]

instance Lude.ToQuery SetDataRetrievalPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetDataRetrievalPolicyResponse' smart constructor.
data SetDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDataRetrievalPolicyResponse' with the minimum fields required to make a request.
mkSetDataRetrievalPolicyResponse ::
  SetDataRetrievalPolicyResponse
mkSetDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'
