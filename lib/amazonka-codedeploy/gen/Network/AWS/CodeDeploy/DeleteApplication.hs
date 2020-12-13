{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
module Network.AWS.CodeDeploy.DeleteApplication
  ( -- * Creating a request
    DeleteApplication (..),
    mkDeleteApplication,

    -- ** Request lenses
    daApplicationName,

    -- * Destructuring the response
    DeleteApplicationResponse (..),
    mkDeleteApplicationResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteApplication@ operation.
--
-- /See:/ 'mkDeleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
mkDeleteApplication ::
  -- | 'applicationName'
  Lude.Text ->
  DeleteApplication
mkDeleteApplication pApplicationName_ =
  DeleteApplication' {applicationName = pApplicationName_}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationName :: Lens.Lens' DeleteApplication Lude.Text
daApplicationName = Lens.lens (applicationName :: DeleteApplication -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplication)
{-# DEPRECATED daApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull DeleteApplicationResponse'

instance Lude.ToHeaders DeleteApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.DeleteApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("applicationName" Lude..= applicationName)]
      )

instance Lude.ToPath DeleteApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
mkDeleteApplicationResponse ::
  DeleteApplicationResponse
mkDeleteApplicationResponse = DeleteApplicationResponse'
