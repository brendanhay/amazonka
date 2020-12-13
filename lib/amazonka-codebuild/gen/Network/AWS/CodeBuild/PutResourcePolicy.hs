{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a resource policy for the ARN of a @Project@ or @ReportGroup@ object.
module Network.AWS.CodeBuild.PutResourcePolicy
  ( -- * Creating a request
    PutResourcePolicy (..),
    mkPutResourcePolicy,

    -- ** Request lenses
    prpResourceARN,
    prpPolicy,

    -- * Destructuring the response
    PutResourcePolicyResponse (..),
    mkPutResourcePolicyResponse,

    -- ** Response lenses
    prprsResourceARN,
    prprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy.
    resourceARN :: Lude.Text,
    -- | A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ .
    policy :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy.
-- * 'policy' - A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ .
mkPutResourcePolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  PutResourcePolicy
mkPutResourcePolicy pResourceARN_ pPolicy_ =
  PutResourcePolicy'
    { resourceARN = pResourceARN_,
      policy = pPolicy_
    }

-- | The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpResourceARN :: Lens.Lens' PutResourcePolicy Lude.Text
prpResourceARN = Lens.lens (resourceARN :: PutResourcePolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: PutResourcePolicy)
{-# DEPRECATED prpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicy :: Lens.Lens' PutResourcePolicy Lude.Text
prpPolicy = Lens.lens (policy :: PutResourcePolicy -> Lude.Text) (\s a -> s {policy = a} :: PutResourcePolicy)
{-# DEPRECATED prpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Lude.<$> (x Lude..?> "resourceArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.PutResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceArn" Lude..= resourceARN),
            Lude.Just ("policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy.
-- * 'responseStatus' - The response status code.
mkPutResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutResourcePolicyResponse
mkPutResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    { resourceARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResourceARN :: Lens.Lens' PutResourcePolicyResponse (Lude.Maybe Lude.Text)
prprsResourceARN = Lens.lens (resourceARN :: PutResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Lude.Int
prprsResponseStatus = Lens.lens (responseStatus :: PutResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
