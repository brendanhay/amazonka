{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetRepositoryTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about triggers configured for a repository.
module Network.AWS.CodeCommit.GetRepositoryTriggers
  ( -- * Creating a request
    GetRepositoryTriggers (..),
    mkGetRepositoryTriggers,

    -- ** Request lenses
    grtRepositoryName,

    -- * Destructuring the response
    GetRepositoryTriggersResponse (..),
    mkGetRepositoryTriggersResponse,

    -- ** Response lenses
    grtrsConfigurationId,
    grtrsTriggers,
    grtrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a get repository triggers operation.
--
-- /See:/ 'mkGetRepositoryTriggers' smart constructor.
newtype GetRepositoryTriggers = GetRepositoryTriggers'
  { repositoryName ::
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

-- | Creates a value of 'GetRepositoryTriggers' with the minimum fields required to make a request.
--
-- * 'repositoryName' - The name of the repository for which the trigger is configured.
mkGetRepositoryTriggers ::
  -- | 'repositoryName'
  Lude.Text ->
  GetRepositoryTriggers
mkGetRepositoryTriggers pRepositoryName_ =
  GetRepositoryTriggers' {repositoryName = pRepositoryName_}

-- | The name of the repository for which the trigger is configured.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtRepositoryName :: Lens.Lens' GetRepositoryTriggers Lude.Text
grtRepositoryName = Lens.lens (repositoryName :: GetRepositoryTriggers -> Lude.Text) (\s a -> s {repositoryName = a} :: GetRepositoryTriggers)
{-# DEPRECATED grtRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetRepositoryTriggers where
  type Rs GetRepositoryTriggers = GetRepositoryTriggersResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRepositoryTriggersResponse'
            Lude.<$> (x Lude..?> "configurationId")
            Lude.<*> (x Lude..?> "triggers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRepositoryTriggers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetRepositoryTriggers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRepositoryTriggers where
  toJSON GetRepositoryTriggers' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("repositoryName" Lude..= repositoryName)]
      )

instance Lude.ToPath GetRepositoryTriggers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRepositoryTriggers where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a get repository triggers operation.
--
-- /See:/ 'mkGetRepositoryTriggersResponse' smart constructor.
data GetRepositoryTriggersResponse = GetRepositoryTriggersResponse'
  { configurationId ::
      Lude.Maybe Lude.Text,
    triggers ::
      Lude.Maybe [RepositoryTrigger],
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

-- | Creates a value of 'GetRepositoryTriggersResponse' with the minimum fields required to make a request.
--
-- * 'configurationId' - The system-generated unique ID for the trigger.
-- * 'responseStatus' - The response status code.
-- * 'triggers' - The JSON block of configuration information for each trigger.
mkGetRepositoryTriggersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRepositoryTriggersResponse
mkGetRepositoryTriggersResponse pResponseStatus_ =
  GetRepositoryTriggersResponse'
    { configurationId = Lude.Nothing,
      triggers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The system-generated unique ID for the trigger.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtrsConfigurationId :: Lens.Lens' GetRepositoryTriggersResponse (Lude.Maybe Lude.Text)
grtrsConfigurationId = Lens.lens (configurationId :: GetRepositoryTriggersResponse -> Lude.Maybe Lude.Text) (\s a -> s {configurationId = a} :: GetRepositoryTriggersResponse)
{-# DEPRECATED grtrsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The JSON block of configuration information for each trigger.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtrsTriggers :: Lens.Lens' GetRepositoryTriggersResponse (Lude.Maybe [RepositoryTrigger])
grtrsTriggers = Lens.lens (triggers :: GetRepositoryTriggersResponse -> Lude.Maybe [RepositoryTrigger]) (\s a -> s {triggers = a} :: GetRepositoryTriggersResponse)
{-# DEPRECATED grtrsTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtrsResponseStatus :: Lens.Lens' GetRepositoryTriggersResponse Lude.Int
grtrsResponseStatus = Lens.lens (responseStatus :: GetRepositoryTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRepositoryTriggersResponse)
{-# DEPRECATED grtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
