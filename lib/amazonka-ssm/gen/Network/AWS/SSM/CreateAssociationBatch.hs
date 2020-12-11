{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Systems Manager document with the specified instances or targets.
--
-- When you associate a document with one or more instances using instance IDs or tags, SSM Agent running on the instance processes the document and configures the instance as specified.
-- If you associate a document with an instance that already has an associated document, the system returns the AssociationAlreadyExists exception.
module Network.AWS.SSM.CreateAssociationBatch
  ( -- * Creating a request
    CreateAssociationBatch (..),
    mkCreateAssociationBatch,

    -- ** Request lenses
    cabEntries,

    -- * Destructuring the response
    CreateAssociationBatchResponse (..),
    mkCreateAssociationBatchResponse,

    -- ** Response lenses
    cabrsSuccessful,
    cabrsFailed,
    cabrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateAssociationBatch' smart constructor.
newtype CreateAssociationBatch = CreateAssociationBatch'
  { entries ::
      Lude.NonEmpty
        CreateAssociationBatchRequestEntry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAssociationBatch' with the minimum fields required to make a request.
--
-- * 'entries' - One or more associations.
mkCreateAssociationBatch ::
  -- | 'entries'
  Lude.NonEmpty CreateAssociationBatchRequestEntry ->
  CreateAssociationBatch
mkCreateAssociationBatch pEntries_ =
  CreateAssociationBatch' {entries = pEntries_}

-- | One or more associations.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabEntries :: Lens.Lens' CreateAssociationBatch (Lude.NonEmpty CreateAssociationBatchRequestEntry)
cabEntries = Lens.lens (entries :: CreateAssociationBatch -> Lude.NonEmpty CreateAssociationBatchRequestEntry) (\s a -> s {entries = a} :: CreateAssociationBatch)
{-# DEPRECATED cabEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Lude.AWSRequest CreateAssociationBatch where
  type Rs CreateAssociationBatch = CreateAssociationBatchResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAssociationBatchResponse'
            Lude.<$> (x Lude..?> "Successful" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Failed" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAssociationBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateAssociationBatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAssociationBatch where
  toJSON CreateAssociationBatch' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Entries" Lude..= entries)])

instance Lude.ToPath CreateAssociationBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAssociationBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAssociationBatchResponse' smart constructor.
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
  { successful ::
      Lude.Maybe
        [AssociationDescription],
    failed ::
      Lude.Maybe
        [FailedCreateAssociation],
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

-- | Creates a value of 'CreateAssociationBatchResponse' with the minimum fields required to make a request.
--
-- * 'failed' - Information about the associations that failed.
-- * 'responseStatus' - The response status code.
-- * 'successful' - Information about the associations that succeeded.
mkCreateAssociationBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAssociationBatchResponse
mkCreateAssociationBatchResponse pResponseStatus_ =
  CreateAssociationBatchResponse'
    { successful = Lude.Nothing,
      failed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the associations that succeeded.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrsSuccessful :: Lens.Lens' CreateAssociationBatchResponse (Lude.Maybe [AssociationDescription])
cabrsSuccessful = Lens.lens (successful :: CreateAssociationBatchResponse -> Lude.Maybe [AssociationDescription]) (\s a -> s {successful = a} :: CreateAssociationBatchResponse)
{-# DEPRECATED cabrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | Information about the associations that failed.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrsFailed :: Lens.Lens' CreateAssociationBatchResponse (Lude.Maybe [FailedCreateAssociation])
cabrsFailed = Lens.lens (failed :: CreateAssociationBatchResponse -> Lude.Maybe [FailedCreateAssociation]) (\s a -> s {failed = a} :: CreateAssociationBatchResponse)
{-# DEPRECATED cabrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrsResponseStatus :: Lens.Lens' CreateAssociationBatchResponse Lude.Int
cabrsResponseStatus = Lens.lens (responseStatus :: CreateAssociationBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAssociationBatchResponse)
{-# DEPRECATED cabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
