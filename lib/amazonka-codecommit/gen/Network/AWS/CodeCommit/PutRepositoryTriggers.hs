{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PutRepositoryTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces all triggers for a repository. Used to create or delete triggers.
module Network.AWS.CodeCommit.PutRepositoryTriggers
  ( -- * Creating a request
    PutRepositoryTriggers (..),
    mkPutRepositoryTriggers,

    -- ** Request lenses
    pRepositoryName,
    pTriggers,

    -- * Destructuring the response
    PutRepositoryTriggersResponse (..),
    mkPutRepositoryTriggersResponse,

    -- ** Response lenses
    prtrsConfigurationId,
    prtrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a put repository triggers operation.
--
-- /See:/ 'mkPutRepositoryTriggers' smart constructor.
data PutRepositoryTriggers = PutRepositoryTriggers'
  { repositoryName ::
      Lude.Text,
    triggers :: [RepositoryTrigger]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRepositoryTriggers' with the minimum fields required to make a request.
--
-- * 'repositoryName' - The name of the repository where you want to create or update the trigger.
-- * 'triggers' - The JSON block of configuration information for each trigger.
mkPutRepositoryTriggers ::
  -- | 'repositoryName'
  Lude.Text ->
  PutRepositoryTriggers
mkPutRepositoryTriggers pRepositoryName_ =
  PutRepositoryTriggers'
    { repositoryName = pRepositoryName_,
      triggers = Lude.mempty
    }

-- | The name of the repository where you want to create or update the trigger.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRepositoryName :: Lens.Lens' PutRepositoryTriggers Lude.Text
pRepositoryName = Lens.lens (repositoryName :: PutRepositoryTriggers -> Lude.Text) (\s a -> s {repositoryName = a} :: PutRepositoryTriggers)
{-# DEPRECATED pRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON block of configuration information for each trigger.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTriggers :: Lens.Lens' PutRepositoryTriggers [RepositoryTrigger]
pTriggers = Lens.lens (triggers :: PutRepositoryTriggers -> [RepositoryTrigger]) (\s a -> s {triggers = a} :: PutRepositoryTriggers)
{-# DEPRECATED pTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

instance Lude.AWSRequest PutRepositoryTriggers where
  type Rs PutRepositoryTriggers = PutRepositoryTriggersResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRepositoryTriggersResponse'
            Lude.<$> (x Lude..?> "configurationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutRepositoryTriggers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.PutRepositoryTriggers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRepositoryTriggers where
  toJSON PutRepositoryTriggers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("triggers" Lude..= triggers)
          ]
      )

instance Lude.ToPath PutRepositoryTriggers where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRepositoryTriggers where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a put repository triggers operation.
--
-- /See:/ 'mkPutRepositoryTriggersResponse' smart constructor.
data PutRepositoryTriggersResponse = PutRepositoryTriggersResponse'
  { configurationId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PutRepositoryTriggersResponse' with the minimum fields required to make a request.
--
-- * 'configurationId' - The system-generated unique ID for the create or update operation.
-- * 'responseStatus' - The response status code.
mkPutRepositoryTriggersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutRepositoryTriggersResponse
mkPutRepositoryTriggersResponse pResponseStatus_ =
  PutRepositoryTriggersResponse'
    { configurationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The system-generated unique ID for the create or update operation.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtrsConfigurationId :: Lens.Lens' PutRepositoryTriggersResponse (Lude.Maybe Lude.Text)
prtrsConfigurationId = Lens.lens (configurationId :: PutRepositoryTriggersResponse -> Lude.Maybe Lude.Text) (\s a -> s {configurationId = a} :: PutRepositoryTriggersResponse)
{-# DEPRECATED prtrsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtrsResponseStatus :: Lens.Lens' PutRepositoryTriggersResponse Lude.Int
prtrsResponseStatus = Lens.lens (responseStatus :: PutRepositoryTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRepositoryTriggersResponse)
{-# DEPRECATED prtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
