{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLaunchTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more versions of a launch template. You cannot delete the default version of a launch template; you must first assign a different version as the default. If the default version is the only version for the launch template, you must delete the entire launch template using 'DeleteLaunchTemplate' .
module Network.AWS.EC2.DeleteLaunchTemplateVersions
  ( -- * Creating a request
    DeleteLaunchTemplateVersions (..),
    mkDeleteLaunchTemplateVersions,

    -- ** Request lenses
    dltvLaunchTemplateName,
    dltvLaunchTemplateId,
    dltvDryRun,
    dltvVersions,

    -- * Destructuring the response
    DeleteLaunchTemplateVersionsResponse (..),
    mkDeleteLaunchTemplateVersionsResponse,

    -- ** Response lenses
    dltvsrsSuccessfullyDeletedLaunchTemplateVersions,
    dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions,
    dltvsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLaunchTemplateVersions' smart constructor.
data DeleteLaunchTemplateVersions = DeleteLaunchTemplateVersions'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    launchTemplateId ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    versions :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLaunchTemplateVersions' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'launchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
-- * 'launchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
-- * 'versions' - The version numbers of one or more launch template versions to delete.
mkDeleteLaunchTemplateVersions ::
  DeleteLaunchTemplateVersions
mkDeleteLaunchTemplateVersions =
  DeleteLaunchTemplateVersions'
    { launchTemplateName = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      dryRun = Lude.Nothing,
      versions = Lude.mempty
    }

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvLaunchTemplateName = Lens.lens (launchTemplateName :: DeleteLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersions)
{-# DEPRECATED dltvLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvLaunchTemplateId = Lens.lens (launchTemplateId :: DeleteLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersions)
{-# DEPRECATED dltvLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvDryRun :: Lens.Lens' DeleteLaunchTemplateVersions (Lude.Maybe Lude.Bool)
dltvDryRun = Lens.lens (dryRun :: DeleteLaunchTemplateVersions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteLaunchTemplateVersions)
{-# DEPRECATED dltvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The version numbers of one or more launch template versions to delete.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvVersions :: Lens.Lens' DeleteLaunchTemplateVersions [Lude.Text]
dltvVersions = Lens.lens (versions :: DeleteLaunchTemplateVersions -> [Lude.Text]) (\s a -> s {versions = a} :: DeleteLaunchTemplateVersions)
{-# DEPRECATED dltvVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

instance Lude.AWSRequest DeleteLaunchTemplateVersions where
  type
    Rs DeleteLaunchTemplateVersions =
      DeleteLaunchTemplateVersionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateVersionsResponse'
            Lude.<$> ( x Lude..@? "successfullyDeletedLaunchTemplateVersionSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "unsuccessfullyDeletedLaunchTemplateVersionSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLaunchTemplateVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLaunchTemplateVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLaunchTemplateVersions where
  toQuery DeleteLaunchTemplateVersions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteLaunchTemplateVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "LaunchTemplateVersion" versions
      ]

-- | /See:/ 'mkDeleteLaunchTemplateVersionsResponse' smart constructor.
data DeleteLaunchTemplateVersionsResponse = DeleteLaunchTemplateVersionsResponse'
  { successfullyDeletedLaunchTemplateVersions ::
      Lude.Maybe
        [DeleteLaunchTemplateVersionsResponseSuccessItem],
    unsuccessfullyDeletedLaunchTemplateVersions ::
      Lude.Maybe
        [DeleteLaunchTemplateVersionsResponseErrorItem],
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

-- | Creates a value of 'DeleteLaunchTemplateVersionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'successfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that were successfully deleted.
-- * 'unsuccessfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that could not be deleted.
mkDeleteLaunchTemplateVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLaunchTemplateVersionsResponse
mkDeleteLaunchTemplateVersionsResponse pResponseStatus_ =
  DeleteLaunchTemplateVersionsResponse'
    { successfullyDeletedLaunchTemplateVersions =
        Lude.Nothing,
      unsuccessfullyDeletedLaunchTemplateVersions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the launch template versions that were successfully deleted.
--
-- /Note:/ Consider using 'successfullyDeletedLaunchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsrsSuccessfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Lude.Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem])
dltvsrsSuccessfullyDeletedLaunchTemplateVersions = Lens.lens (successfullyDeletedLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> Lude.Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem]) (\s a -> s {successfullyDeletedLaunchTemplateVersions = a} :: DeleteLaunchTemplateVersionsResponse)
{-# DEPRECATED dltvsrsSuccessfullyDeletedLaunchTemplateVersions "Use generic-lens or generic-optics with 'successfullyDeletedLaunchTemplateVersions' instead." #-}

-- | Information about the launch template versions that could not be deleted.
--
-- /Note:/ Consider using 'unsuccessfullyDeletedLaunchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Lude.Maybe [DeleteLaunchTemplateVersionsResponseErrorItem])
dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions = Lens.lens (unsuccessfullyDeletedLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> Lude.Maybe [DeleteLaunchTemplateVersionsResponseErrorItem]) (\s a -> s {unsuccessfullyDeletedLaunchTemplateVersions = a} :: DeleteLaunchTemplateVersionsResponse)
{-# DEPRECATED dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions "Use generic-lens or generic-optics with 'unsuccessfullyDeletedLaunchTemplateVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsrsResponseStatus :: Lens.Lens' DeleteLaunchTemplateVersionsResponse Lude.Int
dltvsrsResponseStatus = Lens.lens (responseStatus :: DeleteLaunchTemplateVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLaunchTemplateVersionsResponse)
{-# DEPRECATED dltvsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
