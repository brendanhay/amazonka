{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all trigger resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListTriggers
  ( -- * Creating a request
    ListTriggers (..),
    mkListTriggers,

    -- ** Request lenses
    ltNextToken,
    ltMaxResults,
    ltTags,
    ltDependentJobName,

    -- * Destructuring the response
    ListTriggersResponse (..),
    mkListTriggersResponse,

    -- ** Response lenses
    ltrsTriggerNames,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTriggers' smart constructor.
data ListTriggers = ListTriggers'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    dependentJobName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTriggers' with the minimum fields required to make a request.
--
-- * 'dependentJobName' - The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
-- * 'maxResults' - The maximum size of a list to return.
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'tags' - Specifies to return only these tagged resources.
mkListTriggers ::
  ListTriggers
mkListTriggers =
  ListTriggers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      tags = Lude.Nothing,
      dependentJobName = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTriggers (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTriggers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTriggers)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTriggers (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTriggers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTriggers)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTags :: Lens.Lens' ListTriggers (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ltTags = Lens.lens (tags :: ListTriggers -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListTriggers)
{-# DEPRECATED ltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
--
-- /Note:/ Consider using 'dependentJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDependentJobName :: Lens.Lens' ListTriggers (Lude.Maybe Lude.Text)
ltDependentJobName = Lens.lens (dependentJobName :: ListTriggers -> Lude.Maybe Lude.Text) (\s a -> s {dependentJobName = a} :: ListTriggers)
{-# DEPRECATED ltDependentJobName "Use generic-lens or generic-optics with 'dependentJobName' instead." #-}

instance Lude.AWSRequest ListTriggers where
  type Rs ListTriggers = ListTriggersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTriggersResponse'
            Lude.<$> (x Lude..?> "TriggerNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTriggers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListTriggers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTriggers where
  toJSON ListTriggers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("Tags" Lude..=) Lude.<$> tags,
            ("DependentJobName" Lude..=) Lude.<$> dependentJobName
          ]
      )

instance Lude.ToPath ListTriggers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTriggers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTriggersResponse' smart constructor.
data ListTriggersResponse = ListTriggersResponse'
  { triggerNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTriggersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the returned list does not contain the last metric available.
-- * 'responseStatus' - The response status code.
-- * 'triggerNames' - The names of all triggers in the account, or the triggers with the specified tags.
mkListTriggersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTriggersResponse
mkListTriggersResponse pResponseStatus_ =
  ListTriggersResponse'
    { triggerNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of all triggers in the account, or the triggers with the specified tags.
--
-- /Note:/ Consider using 'triggerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTriggerNames :: Lens.Lens' ListTriggersResponse (Lude.Maybe [Lude.Text])
ltrsTriggerNames = Lens.lens (triggerNames :: ListTriggersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {triggerNames = a} :: ListTriggersResponse)
{-# DEPRECATED ltrsTriggerNames "Use generic-lens or generic-optics with 'triggerNames' instead." #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTriggersResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTriggersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTriggersResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTriggersResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTriggersResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
