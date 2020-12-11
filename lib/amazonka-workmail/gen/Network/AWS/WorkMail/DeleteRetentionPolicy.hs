{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy from the specified organization.
module Network.AWS.WorkMail.DeleteRetentionPolicy
  ( -- * Creating a request
    DeleteRetentionPolicy (..),
    mkDeleteRetentionPolicy,

    -- ** Request lenses
    drpOrganizationId,
    drpId,

    -- * Destructuring the response
    DeleteRetentionPolicyResponse (..),
    mkDeleteRetentionPolicyResponse,

    -- ** Response lenses
    drprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { organizationId ::
      Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRetentionPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The retention policy ID.
-- * 'organizationId' - The organization ID.
mkDeleteRetentionPolicy ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  DeleteRetentionPolicy
mkDeleteRetentionPolicy pOrganizationId_ pId_ =
  DeleteRetentionPolicy'
    { organizationId = pOrganizationId_,
      id = pId_
    }

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpOrganizationId :: Lens.Lens' DeleteRetentionPolicy Lude.Text
drpOrganizationId = Lens.lens (organizationId :: DeleteRetentionPolicy -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteRetentionPolicy)
{-# DEPRECATED drpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpId :: Lens.Lens' DeleteRetentionPolicy Lude.Text
drpId = Lens.lens (id :: DeleteRetentionPolicy -> Lude.Text) (\s a -> s {id = a} :: DeleteRetentionPolicy)
{-# DEPRECATED drpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteRetentionPolicy where
  type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRetentionPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRetentionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteRetentionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DeleteRetentionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRetentionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRetentionPolicyResponse' smart constructor.
newtype DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRetentionPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRetentionPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRetentionPolicyResponse
mkDeleteRetentionPolicyResponse pResponseStatus_ =
  DeleteRetentionPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DeleteRetentionPolicyResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DeleteRetentionPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRetentionPolicyResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
