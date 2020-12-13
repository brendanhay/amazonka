{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource policy from this account. This revokes the access of the identities in that policy to put log events to this account.
module Network.AWS.CloudWatchLogs.DeleteResourcePolicy
  ( -- * Creating a request
    DeleteResourcePolicy (..),
    mkDeleteResourcePolicy,

    -- ** Request lenses
    drpPolicyName,

    -- * Destructuring the response
    DeleteResourcePolicyResponse (..),
    mkDeleteResourcePolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResourcePolicy' smart constructor.
newtype DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The name of the policy to be revoked. This parameter is required.
    policyName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy to be revoked. This parameter is required.
mkDeleteResourcePolicy ::
  DeleteResourcePolicy
mkDeleteResourcePolicy =
  DeleteResourcePolicy' {policyName = Lude.Nothing}

-- | The name of the policy to be revoked. This parameter is required.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPolicyName :: Lens.Lens' DeleteResourcePolicy (Lude.Maybe Lude.Text)
drpPolicyName = Lens.lens (policyName :: DeleteResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: DeleteResourcePolicy)
{-# DEPRECATED drpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest DeleteResourcePolicy where
  type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteResourcePolicyResponse'

instance Lude.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Lude.object
      (Lude.catMaybes [("policyName" Lude..=) Lude.<$> policyName])

instance Lude.ToPath DeleteResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcePolicyResponse' with the minimum fields required to make a request.
mkDeleteResourcePolicyResponse ::
  DeleteResourcePolicyResponse
mkDeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
