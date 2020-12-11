{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.PutRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a retention policy to the specified organization.
module Network.AWS.WorkMail.PutRetentionPolicy
  ( -- * Creating a request
    PutRetentionPolicy (..),
    mkPutRetentionPolicy,

    -- ** Request lenses
    prpId,
    prpDescription,
    prpOrganizationId,
    prpName,
    prpFolderConfigurations,

    -- * Destructuring the response
    PutRetentionPolicyResponse (..),
    mkPutRetentionPolicyResponse,

    -- ** Response lenses
    prprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { id ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    organizationId :: Lude.Text,
    name :: Lude.Text,
    folderConfigurations :: [FolderConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRetentionPolicy' with the minimum fields required to make a request.
--
-- * 'description' - The retention policy description.
-- * 'folderConfigurations' - The retention policy folder configurations.
-- * 'id' - The retention policy ID.
-- * 'name' - The retention policy name.
-- * 'organizationId' - The organization ID.
mkPutRetentionPolicy ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  PutRetentionPolicy
mkPutRetentionPolicy pOrganizationId_ pName_ =
  PutRetentionPolicy'
    { id = Lude.Nothing,
      description = Lude.Nothing,
      organizationId = pOrganizationId_,
      name = pName_,
      folderConfigurations = Lude.mempty
    }

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpId :: Lens.Lens' PutRetentionPolicy (Lude.Maybe Lude.Text)
prpId = Lens.lens (id :: PutRetentionPolicy -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PutRetentionPolicy)
{-# DEPRECATED prpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The retention policy description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpDescription :: Lens.Lens' PutRetentionPolicy (Lude.Maybe (Lude.Sensitive Lude.Text))
prpDescription = Lens.lens (description :: PutRetentionPolicy -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: PutRetentionPolicy)
{-# DEPRECATED prpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpOrganizationId :: Lens.Lens' PutRetentionPolicy Lude.Text
prpOrganizationId = Lens.lens (organizationId :: PutRetentionPolicy -> Lude.Text) (\s a -> s {organizationId = a} :: PutRetentionPolicy)
{-# DEPRECATED prpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The retention policy name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpName :: Lens.Lens' PutRetentionPolicy Lude.Text
prpName = Lens.lens (name :: PutRetentionPolicy -> Lude.Text) (\s a -> s {name = a} :: PutRetentionPolicy)
{-# DEPRECATED prpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The retention policy folder configurations.
--
-- /Note:/ Consider using 'folderConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpFolderConfigurations :: Lens.Lens' PutRetentionPolicy [FolderConfiguration]
prpFolderConfigurations = Lens.lens (folderConfigurations :: PutRetentionPolicy -> [FolderConfiguration]) (\s a -> s {folderConfigurations = a} :: PutRetentionPolicy)
{-# DEPRECATED prpFolderConfigurations "Use generic-lens or generic-optics with 'folderConfigurations' instead." #-}

instance Lude.AWSRequest PutRetentionPolicy where
  type Rs PutRetentionPolicy = PutRetentionPolicyResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutRetentionPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutRetentionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.PutRetentionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Id" Lude..=) Lude.<$> id,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("FolderConfigurations" Lude..= folderConfigurations)
          ]
      )

instance Lude.ToPath PutRetentionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRetentionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRetentionPolicyResponse' smart constructor.
newtype PutRetentionPolicyResponse = PutRetentionPolicyResponse'
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

-- | Creates a value of 'PutRetentionPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutRetentionPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutRetentionPolicyResponse
mkPutRetentionPolicyResponse pResponseStatus_ =
  PutRetentionPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResponseStatus :: Lens.Lens' PutRetentionPolicyResponse Lude.Int
prprsResponseStatus = Lens.lens (responseStatus :: PutRetentionPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRetentionPolicyResponse)
{-# DEPRECATED prprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
