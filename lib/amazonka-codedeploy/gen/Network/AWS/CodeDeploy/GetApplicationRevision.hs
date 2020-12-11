{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetApplicationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application revision.
module Network.AWS.CodeDeploy.GetApplicationRevision
  ( -- * Creating a request
    GetApplicationRevision (..),
    mkGetApplicationRevision,

    -- ** Request lenses
    garApplicationName,
    garRevision,

    -- * Destructuring the response
    GetApplicationRevisionResponse (..),
    mkGetApplicationRevisionResponse,

    -- ** Response lenses
    garrsApplicationName,
    garrsRevisionInfo,
    garrsRevision,
    garrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetApplicationRevision@ operation.
--
-- /See:/ 'mkGetApplicationRevision' smart constructor.
data GetApplicationRevision = GetApplicationRevision'
  { applicationName ::
      Lude.Text,
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

-- | Creates a value of 'GetApplicationRevision' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application that corresponds to the revision.
-- * 'revision' - Information about the application revision to get, including type and location.
mkGetApplicationRevision ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'revision'
  RevisionLocation ->
  GetApplicationRevision
mkGetApplicationRevision pApplicationName_ pRevision_ =
  GetApplicationRevision'
    { applicationName = pApplicationName_,
      revision = pRevision_
    }

-- | The name of the application that corresponds to the revision.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garApplicationName :: Lens.Lens' GetApplicationRevision Lude.Text
garApplicationName = Lens.lens (applicationName :: GetApplicationRevision -> Lude.Text) (\s a -> s {applicationName = a} :: GetApplicationRevision)
{-# DEPRECATED garApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information about the application revision to get, including type and location.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garRevision :: Lens.Lens' GetApplicationRevision RevisionLocation
garRevision = Lens.lens (revision :: GetApplicationRevision -> RevisionLocation) (\s a -> s {revision = a} :: GetApplicationRevision)
{-# DEPRECATED garRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

instance Lude.AWSRequest GetApplicationRevision where
  type Rs GetApplicationRevision = GetApplicationRevisionResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApplicationRevisionResponse'
            Lude.<$> (x Lude..?> "applicationName")
            Lude.<*> (x Lude..?> "revisionInfo")
            Lude.<*> (x Lude..?> "revision")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApplicationRevision where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetApplicationRevision" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetApplicationRevision where
  toJSON GetApplicationRevision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("applicationName" Lude..= applicationName),
            Lude.Just ("revision" Lude..= revision)
          ]
      )

instance Lude.ToPath GetApplicationRevision where
  toPath = Lude.const "/"

instance Lude.ToQuery GetApplicationRevision where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetApplicationRevision@ operation.
--
-- /See:/ 'mkGetApplicationRevisionResponse' smart constructor.
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'
  { applicationName ::
      Lude.Maybe Lude.Text,
    revisionInfo ::
      Lude.Maybe
        GenericRevisionInfo,
    revision ::
      Lude.Maybe RevisionLocation,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationRevisionResponse' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application that corresponds to the revision.
-- * 'responseStatus' - The response status code.
-- * 'revision' - Additional information about the revision, including type and location.
-- * 'revisionInfo' - General information about the revision.
mkGetApplicationRevisionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetApplicationRevisionResponse
mkGetApplicationRevisionResponse pResponseStatus_ =
  GetApplicationRevisionResponse'
    { applicationName = Lude.Nothing,
      revisionInfo = Lude.Nothing,
      revision = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the application that corresponds to the revision.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsApplicationName :: Lens.Lens' GetApplicationRevisionResponse (Lude.Maybe Lude.Text)
garrsApplicationName = Lens.lens (applicationName :: GetApplicationRevisionResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: GetApplicationRevisionResponse)
{-# DEPRECATED garrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | General information about the revision.
--
-- /Note:/ Consider using 'revisionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsRevisionInfo :: Lens.Lens' GetApplicationRevisionResponse (Lude.Maybe GenericRevisionInfo)
garrsRevisionInfo = Lens.lens (revisionInfo :: GetApplicationRevisionResponse -> Lude.Maybe GenericRevisionInfo) (\s a -> s {revisionInfo = a} :: GetApplicationRevisionResponse)
{-# DEPRECATED garrsRevisionInfo "Use generic-lens or generic-optics with 'revisionInfo' instead." #-}

-- | Additional information about the revision, including type and location.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsRevision :: Lens.Lens' GetApplicationRevisionResponse (Lude.Maybe RevisionLocation)
garrsRevision = Lens.lens (revision :: GetApplicationRevisionResponse -> Lude.Maybe RevisionLocation) (\s a -> s {revision = a} :: GetApplicationRevisionResponse)
{-# DEPRECATED garrsRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetApplicationRevisionResponse Lude.Int
garrsResponseStatus = Lens.lens (responseStatus :: GetApplicationRevisionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApplicationRevisionResponse)
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
