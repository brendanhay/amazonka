{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteAccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access control rule for the specified WorkMail organization.
module Network.AWS.WorkMail.DeleteAccessControlRule
  ( -- * Creating a request
    DeleteAccessControlRule (..),
    mkDeleteAccessControlRule,

    -- ** Request lenses
    dacrName,
    dacrOrganizationId,

    -- * Destructuring the response
    DeleteAccessControlRuleResponse (..),
    mkDeleteAccessControlRuleResponse,

    -- ** Response lenses
    dacrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteAccessControlRule' smart constructor.
data DeleteAccessControlRule = DeleteAccessControlRule'
  { -- | The name of the access control rule.
    name :: Lude.Text,
    -- | The identifier for the organization.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccessControlRule' with the minimum fields required to make a request.
--
-- * 'name' - The name of the access control rule.
-- * 'organizationId' - The identifier for the organization.
mkDeleteAccessControlRule ::
  -- | 'name'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteAccessControlRule
mkDeleteAccessControlRule pName_ pOrganizationId_ =
  DeleteAccessControlRule'
    { name = pName_,
      organizationId = pOrganizationId_
    }

-- | The name of the access control rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrName :: Lens.Lens' DeleteAccessControlRule Lude.Text
dacrName = Lens.lens (name :: DeleteAccessControlRule -> Lude.Text) (\s a -> s {name = a} :: DeleteAccessControlRule)
{-# DEPRECATED dacrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier for the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrOrganizationId :: Lens.Lens' DeleteAccessControlRule Lude.Text
dacrOrganizationId = Lens.lens (organizationId :: DeleteAccessControlRule -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteAccessControlRule)
{-# DEPRECATED dacrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteAccessControlRule where
  type Rs DeleteAccessControlRule = DeleteAccessControlRuleResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAccessControlRuleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAccessControlRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteAccessControlRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAccessControlRule where
  toJSON DeleteAccessControlRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeleteAccessControlRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAccessControlRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAccessControlRuleResponse' smart constructor.
newtype DeleteAccessControlRuleResponse = DeleteAccessControlRuleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccessControlRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAccessControlRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAccessControlRuleResponse
mkDeleteAccessControlRuleResponse pResponseStatus_ =
  DeleteAccessControlRuleResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrrsResponseStatus :: Lens.Lens' DeleteAccessControlRuleResponse Lude.Int
dacrrsResponseStatus = Lens.lens (responseStatus :: DeleteAccessControlRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAccessControlRuleResponse)
{-# DEPRECATED dacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
