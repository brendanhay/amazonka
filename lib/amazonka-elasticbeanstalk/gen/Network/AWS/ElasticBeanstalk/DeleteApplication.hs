{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application along with all associated versions and configurations. The application versions will not be deleted from your Amazon S3 bucket.
module Network.AWS.ElasticBeanstalk.DeleteApplication
  ( -- * Creating a request
    DeleteApplication (..),
    mkDeleteApplication,

    -- ** Request lenses
    daTerminateEnvByForce,
    daApplicationName,

    -- * Destructuring the response
    DeleteApplicationResponse (..),
    mkDeleteApplicationResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete an application.
--
-- /See:/ 'mkDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | When set to true, running environments will be terminated before deleting the application.
    terminateEnvByForce :: Lude.Maybe Lude.Bool,
    -- | The name of the application to delete.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- * 'terminateEnvByForce' - When set to true, running environments will be terminated before deleting the application.
-- * 'applicationName' - The name of the application to delete.
mkDeleteApplication ::
  -- | 'applicationName'
  Lude.Text ->
  DeleteApplication
mkDeleteApplication pApplicationName_ =
  DeleteApplication'
    { terminateEnvByForce = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | When set to true, running environments will be terminated before deleting the application.
--
-- /Note:/ Consider using 'terminateEnvByForce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daTerminateEnvByForce :: Lens.Lens' DeleteApplication (Lude.Maybe Lude.Bool)
daTerminateEnvByForce = Lens.lens (terminateEnvByForce :: DeleteApplication -> Lude.Maybe Lude.Bool) (\s a -> s {terminateEnvByForce = a} :: DeleteApplication)
{-# DEPRECATED daTerminateEnvByForce "Use generic-lens or generic-optics with 'terminateEnvByForce' instead." #-}

-- | The name of the application to delete.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationName :: Lens.Lens' DeleteApplication Lude.Text
daApplicationName = Lens.lens (applicationName :: DeleteApplication -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplication)
{-# DEPRECATED daApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull DeleteApplicationResponse'

instance Lude.ToHeaders DeleteApplication where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplication where
  toQuery DeleteApplication' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteApplication" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TerminateEnvByForce" Lude.=: terminateEnvByForce,
        "ApplicationName" Lude.=: applicationName
      ]

-- | /See:/ 'mkDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
mkDeleteApplicationResponse ::
  DeleteApplicationResponse
mkDeleteApplicationResponse = DeleteApplicationResponse'
