{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current data retrieval policy for the account and region specified in the GET request. For more information about data retrieval policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies> .
module Network.AWS.Glacier.GetDataRetrievalPolicy
  ( -- * Creating a request
    GetDataRetrievalPolicy (..),
    mkGetDataRetrievalPolicy,

    -- ** Request lenses
    gdrpAccountId,

    -- * Destructuring the response
    GetDataRetrievalPolicyResponse (..),
    mkGetDataRetrievalPolicyResponse,

    -- ** Response lenses
    gdrprsPolicy,
    gdrprsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input for GetDataRetrievalPolicy.
--
-- /See:/ 'mkGetDataRetrievalPolicy' smart constructor.
newtype GetDataRetrievalPolicy = GetDataRetrievalPolicy'
  { accountId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataRetrievalPolicy' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
mkGetDataRetrievalPolicy ::
  -- | 'accountId'
  Lude.Text ->
  GetDataRetrievalPolicy
mkGetDataRetrievalPolicy pAccountId_ =
  GetDataRetrievalPolicy' {accountId = pAccountId_}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrpAccountId :: Lens.Lens' GetDataRetrievalPolicy Lude.Text
gdrpAccountId = Lens.lens (accountId :: GetDataRetrievalPolicy -> Lude.Text) (\s a -> s {accountId = a} :: GetDataRetrievalPolicy)
{-# DEPRECATED gdrpAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest GetDataRetrievalPolicy where
  type Rs GetDataRetrievalPolicy = GetDataRetrievalPolicyResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataRetrievalPolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataRetrievalPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetDataRetrievalPolicy where
  toPath GetDataRetrievalPolicy' {..} =
    Lude.mconcat
      ["/", Lude.toBS accountId, "/policies/data-retrieval"]

instance Lude.ToQuery GetDataRetrievalPolicy where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to the @GetDataRetrievalPolicy@ request.
--
-- /See:/ 'mkGetDataRetrievalPolicyResponse' smart constructor.
data GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'
  { policy ::
      Lude.Maybe
        DataRetrievalPolicy,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataRetrievalPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - Contains the returned data retrieval policy in JSON format.
-- * 'responseStatus' - The response status code.
mkGetDataRetrievalPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataRetrievalPolicyResponse
mkGetDataRetrievalPolicyResponse pResponseStatus_ =
  GetDataRetrievalPolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the returned data retrieval policy in JSON format.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsPolicy :: Lens.Lens' GetDataRetrievalPolicyResponse (Lude.Maybe DataRetrievalPolicy)
gdrprsPolicy = Lens.lens (policy :: GetDataRetrievalPolicyResponse -> Lude.Maybe DataRetrievalPolicy) (\s a -> s {policy = a} :: GetDataRetrievalPolicyResponse)
{-# DEPRECATED gdrprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsResponseStatus :: Lens.Lens' GetDataRetrievalPolicyResponse Lude.Int
gdrprsResponseStatus = Lens.lens (responseStatus :: GetDataRetrievalPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataRetrievalPolicyResponse)
{-# DEPRECATED gdrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
