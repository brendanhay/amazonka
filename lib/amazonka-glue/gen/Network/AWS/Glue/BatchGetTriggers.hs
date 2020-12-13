{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of trigger names. After calling the @ListTriggers@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetTriggers
  ( -- * Creating a request
    BatchGetTriggers (..),
    mkBatchGetTriggers,

    -- ** Request lenses
    bgtTriggerNames,

    -- * Destructuring the response
    BatchGetTriggersResponse (..),
    mkBatchGetTriggersResponse,

    -- ** Response lenses
    bgtrsTriggersNotFound,
    bgtrsTriggers,
    bgtrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetTriggers' smart constructor.
newtype BatchGetTriggers = BatchGetTriggers'
  { -- | A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
    triggerNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetTriggers' with the minimum fields required to make a request.
--
-- * 'triggerNames' - A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
mkBatchGetTriggers ::
  BatchGetTriggers
mkBatchGetTriggers = BatchGetTriggers' {triggerNames = Lude.mempty}

-- | A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
--
-- /Note:/ Consider using 'triggerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtTriggerNames :: Lens.Lens' BatchGetTriggers [Lude.Text]
bgtTriggerNames = Lens.lens (triggerNames :: BatchGetTriggers -> [Lude.Text]) (\s a -> s {triggerNames = a} :: BatchGetTriggers)
{-# DEPRECATED bgtTriggerNames "Use generic-lens or generic-optics with 'triggerNames' instead." #-}

instance Lude.AWSRequest BatchGetTriggers where
  type Rs BatchGetTriggers = BatchGetTriggersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetTriggersResponse'
            Lude.<$> (x Lude..?> "TriggersNotFound" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Triggers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetTriggers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchGetTriggers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetTriggers where
  toJSON BatchGetTriggers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TriggerNames" Lude..= triggerNames)])

instance Lude.ToPath BatchGetTriggers where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetTriggers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetTriggersResponse' smart constructor.
data BatchGetTriggersResponse = BatchGetTriggersResponse'
  { -- | A list of names of triggers not found.
    triggersNotFound :: Lude.Maybe [Lude.Text],
    -- | A list of trigger definitions.
    triggers :: Lude.Maybe [Trigger],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetTriggersResponse' with the minimum fields required to make a request.
--
-- * 'triggersNotFound' - A list of names of triggers not found.
-- * 'triggers' - A list of trigger definitions.
-- * 'responseStatus' - The response status code.
mkBatchGetTriggersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetTriggersResponse
mkBatchGetTriggersResponse pResponseStatus_ =
  BatchGetTriggersResponse'
    { triggersNotFound = Lude.Nothing,
      triggers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of names of triggers not found.
--
-- /Note:/ Consider using 'triggersNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsTriggersNotFound :: Lens.Lens' BatchGetTriggersResponse (Lude.Maybe [Lude.Text])
bgtrsTriggersNotFound = Lens.lens (triggersNotFound :: BatchGetTriggersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {triggersNotFound = a} :: BatchGetTriggersResponse)
{-# DEPRECATED bgtrsTriggersNotFound "Use generic-lens or generic-optics with 'triggersNotFound' instead." #-}

-- | A list of trigger definitions.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsTriggers :: Lens.Lens' BatchGetTriggersResponse (Lude.Maybe [Trigger])
bgtrsTriggers = Lens.lens (triggers :: BatchGetTriggersResponse -> Lude.Maybe [Trigger]) (\s a -> s {triggers = a} :: BatchGetTriggersResponse)
{-# DEPRECATED bgtrsTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsResponseStatus :: Lens.Lens' BatchGetTriggersResponse Lude.Int
bgtrsResponseStatus = Lens.lens (responseStatus :: BatchGetTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetTriggersResponse)
{-# DEPRECATED bgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
