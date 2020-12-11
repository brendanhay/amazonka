{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified application.
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
  ( -- * Creating a request
    DeleteApplicationVersion (..),
    mkDeleteApplicationVersion,

    -- ** Request lenses
    davDeleteSourceBundle,
    davApplicationName,
    davVersionLabel,

    -- * Destructuring the response
    DeleteApplicationVersionResponse (..),
    mkDeleteApplicationVersionResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete an application version.
--
-- /See:/ 'mkDeleteApplicationVersion' smart constructor.
data DeleteApplicationVersion = DeleteApplicationVersion'
  { deleteSourceBundle ::
      Lude.Maybe Lude.Bool,
    applicationName :: Lude.Text,
    versionLabel :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationVersion' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application to which the version belongs.
-- * 'deleteSourceBundle' - Set to @true@ to delete the source bundle from your storage bucket. Otherwise, the application version is deleted only from Elastic Beanstalk and the source bundle remains in Amazon S3.
-- * 'versionLabel' - The label of the version to delete.
mkDeleteApplicationVersion ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'versionLabel'
  Lude.Text ->
  DeleteApplicationVersion
mkDeleteApplicationVersion pApplicationName_ pVersionLabel_ =
  DeleteApplicationVersion'
    { deleteSourceBundle = Lude.Nothing,
      applicationName = pApplicationName_,
      versionLabel = pVersionLabel_
    }

-- | Set to @true@ to delete the source bundle from your storage bucket. Otherwise, the application version is deleted only from Elastic Beanstalk and the source bundle remains in Amazon S3.
--
-- /Note:/ Consider using 'deleteSourceBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davDeleteSourceBundle :: Lens.Lens' DeleteApplicationVersion (Lude.Maybe Lude.Bool)
davDeleteSourceBundle = Lens.lens (deleteSourceBundle :: DeleteApplicationVersion -> Lude.Maybe Lude.Bool) (\s a -> s {deleteSourceBundle = a} :: DeleteApplicationVersion)
{-# DEPRECATED davDeleteSourceBundle "Use generic-lens or generic-optics with 'deleteSourceBundle' instead." #-}

-- | The name of the application to which the version belongs.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davApplicationName :: Lens.Lens' DeleteApplicationVersion Lude.Text
davApplicationName = Lens.lens (applicationName :: DeleteApplicationVersion -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplicationVersion)
{-# DEPRECATED davApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The label of the version to delete.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davVersionLabel :: Lens.Lens' DeleteApplicationVersion Lude.Text
davVersionLabel = Lens.lens (versionLabel :: DeleteApplicationVersion -> Lude.Text) (\s a -> s {versionLabel = a} :: DeleteApplicationVersion)
{-# DEPRECATED davVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Lude.AWSRequest DeleteApplicationVersion where
  type Rs DeleteApplicationVersion = DeleteApplicationVersionResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull DeleteApplicationVersionResponse'

instance Lude.ToHeaders DeleteApplicationVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteApplicationVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplicationVersion where
  toQuery DeleteApplicationVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteApplicationVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "DeleteSourceBundle" Lude.=: deleteSourceBundle,
        "ApplicationName" Lude.=: applicationName,
        "VersionLabel" Lude.=: versionLabel
      ]

-- | /See:/ 'mkDeleteApplicationVersionResponse' smart constructor.
data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationVersionResponse' with the minimum fields required to make a request.
mkDeleteApplicationVersionResponse ::
  DeleteApplicationVersionResponse
mkDeleteApplicationVersionResponse =
  DeleteApplicationVersionResponse'
