{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetDefaultRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the default retention policy details for the specified organization.
module Network.AWS.WorkMail.GetDefaultRetentionPolicy
  ( -- * Creating a request
    GetDefaultRetentionPolicy (..),
    mkGetDefaultRetentionPolicy,

    -- ** Request lenses
    gdrpOrganizationId,

    -- * Destructuring the response
    GetDefaultRetentionPolicyResponse (..),
    mkGetDefaultRetentionPolicyResponse,

    -- ** Response lenses
    gdrprsName,
    gdrprsId,
    gdrprsFolderConfigurations,
    gdrprsDescription,
    gdrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkGetDefaultRetentionPolicy' smart constructor.
newtype GetDefaultRetentionPolicy = GetDefaultRetentionPolicy'
  { organizationId ::
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

-- | Creates a value of 'GetDefaultRetentionPolicy' with the minimum fields required to make a request.
--
-- * 'organizationId' - The organization ID.
mkGetDefaultRetentionPolicy ::
  -- | 'organizationId'
  Lude.Text ->
  GetDefaultRetentionPolicy
mkGetDefaultRetentionPolicy pOrganizationId_ =
  GetDefaultRetentionPolicy' {organizationId = pOrganizationId_}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrpOrganizationId :: Lens.Lens' GetDefaultRetentionPolicy Lude.Text
gdrpOrganizationId = Lens.lens (organizationId :: GetDefaultRetentionPolicy -> Lude.Text) (\s a -> s {organizationId = a} :: GetDefaultRetentionPolicy)
{-# DEPRECATED gdrpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest GetDefaultRetentionPolicy where
  type
    Rs GetDefaultRetentionPolicy =
      GetDefaultRetentionPolicyResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDefaultRetentionPolicyResponse'
            Lude.<$> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "FolderConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDefaultRetentionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.GetDefaultRetentionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDefaultRetentionPolicy where
  toJSON GetDefaultRetentionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("OrganizationId" Lude..= organizationId)]
      )

instance Lude.ToPath GetDefaultRetentionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDefaultRetentionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDefaultRetentionPolicyResponse' smart constructor.
data GetDefaultRetentionPolicyResponse = GetDefaultRetentionPolicyResponse'
  { name ::
      Lude.Maybe Lude.Text,
    id ::
      Lude.Maybe Lude.Text,
    folderConfigurations ::
      Lude.Maybe
        [FolderConfiguration],
    description ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDefaultRetentionPolicyResponse' with the minimum fields required to make a request.
--
-- * 'description' - The retention policy description.
-- * 'folderConfigurations' - The retention policy folder configurations.
-- * 'id' - The retention policy ID.
-- * 'name' - The retention policy name.
-- * 'responseStatus' - The response status code.
mkGetDefaultRetentionPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDefaultRetentionPolicyResponse
mkGetDefaultRetentionPolicyResponse pResponseStatus_ =
  GetDefaultRetentionPolicyResponse'
    { name = Lude.Nothing,
      id = Lude.Nothing,
      folderConfigurations = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The retention policy name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsName :: Lens.Lens' GetDefaultRetentionPolicyResponse (Lude.Maybe Lude.Text)
gdrprsName = Lens.lens (name :: GetDefaultRetentionPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetDefaultRetentionPolicyResponse)
{-# DEPRECATED gdrprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsId :: Lens.Lens' GetDefaultRetentionPolicyResponse (Lude.Maybe Lude.Text)
gdrprsId = Lens.lens (id :: GetDefaultRetentionPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetDefaultRetentionPolicyResponse)
{-# DEPRECATED gdrprsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The retention policy folder configurations.
--
-- /Note:/ Consider using 'folderConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsFolderConfigurations :: Lens.Lens' GetDefaultRetentionPolicyResponse (Lude.Maybe [FolderConfiguration])
gdrprsFolderConfigurations = Lens.lens (folderConfigurations :: GetDefaultRetentionPolicyResponse -> Lude.Maybe [FolderConfiguration]) (\s a -> s {folderConfigurations = a} :: GetDefaultRetentionPolicyResponse)
{-# DEPRECATED gdrprsFolderConfigurations "Use generic-lens or generic-optics with 'folderConfigurations' instead." #-}

-- | The retention policy description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsDescription :: Lens.Lens' GetDefaultRetentionPolicyResponse (Lude.Maybe Lude.Text)
gdrprsDescription = Lens.lens (description :: GetDefaultRetentionPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetDefaultRetentionPolicyResponse)
{-# DEPRECATED gdrprsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprsResponseStatus :: Lens.Lens' GetDefaultRetentionPolicyResponse Lude.Int
gdrprsResponseStatus = Lens.lens (responseStatus :: GetDefaultRetentionPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDefaultRetentionPolicyResponse)
{-# DEPRECATED gdrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
