{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers with AWS CodeDeploy a revision for the specified application.
module Network.AWS.CodeDeploy.RegisterApplicationRevision
  ( -- * Creating a request
    RegisterApplicationRevision (..),
    mkRegisterApplicationRevision,

    -- ** Request lenses
    rarDescription,
    rarApplicationName,
    rarRevision,

    -- * Destructuring the response
    RegisterApplicationRevisionResponse (..),
    mkRegisterApplicationRevisionResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a RegisterApplicationRevision operation.
--
-- /See:/ 'mkRegisterApplicationRevision' smart constructor.
data RegisterApplicationRevision = RegisterApplicationRevision'
  { description ::
      Lude.Maybe Lude.Text,
    applicationName :: Lude.Text,
    revision :: RevisionLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterApplicationRevision' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'description' - A comment about the revision.
-- * 'revision' - Information about the application revision to register, including type and location.
mkRegisterApplicationRevision ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'revision'
  RevisionLocation ->
  RegisterApplicationRevision
mkRegisterApplicationRevision pApplicationName_ pRevision_ =
  RegisterApplicationRevision'
    { description = Lude.Nothing,
      applicationName = pApplicationName_,
      revision = pRevision_
    }

-- | A comment about the revision.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarDescription :: Lens.Lens' RegisterApplicationRevision (Lude.Maybe Lude.Text)
rarDescription = Lens.lens (description :: RegisterApplicationRevision -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegisterApplicationRevision)
{-# DEPRECATED rarDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarApplicationName :: Lens.Lens' RegisterApplicationRevision Lude.Text
rarApplicationName = Lens.lens (applicationName :: RegisterApplicationRevision -> Lude.Text) (\s a -> s {applicationName = a} :: RegisterApplicationRevision)
{-# DEPRECATED rarApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information about the application revision to register, including type and location.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarRevision :: Lens.Lens' RegisterApplicationRevision RevisionLocation
rarRevision = Lens.lens (revision :: RegisterApplicationRevision -> RevisionLocation) (\s a -> s {revision = a} :: RegisterApplicationRevision)
{-# DEPRECATED rarRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

instance Lude.AWSRequest RegisterApplicationRevision where
  type
    Rs RegisterApplicationRevision =
      RegisterApplicationRevisionResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull RegisterApplicationRevisionResponse'

instance Lude.ToHeaders RegisterApplicationRevision where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.RegisterApplicationRevision" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterApplicationRevision where
  toJSON RegisterApplicationRevision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("description" Lude..=) Lude.<$> description,
            Lude.Just ("applicationName" Lude..= applicationName),
            Lude.Just ("revision" Lude..= revision)
          ]
      )

instance Lude.ToPath RegisterApplicationRevision where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterApplicationRevision where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse = RegisterApplicationRevisionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterApplicationRevisionResponse' with the minimum fields required to make a request.
mkRegisterApplicationRevisionResponse ::
  RegisterApplicationRevisionResponse
mkRegisterApplicationRevisionResponse =
  RegisterApplicationRevisionResponse'
