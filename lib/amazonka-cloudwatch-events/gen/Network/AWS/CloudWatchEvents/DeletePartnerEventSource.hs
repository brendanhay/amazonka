{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeletePartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation is used by SaaS partners to delete a partner event source. This operation is not used by AWS customers.
--
-- When you delete an event source, the status of the corresponding partner event bus in the AWS customer account becomes DELETED.
module Network.AWS.CloudWatchEvents.DeletePartnerEventSource
  ( -- * Creating a request
    DeletePartnerEventSource (..),
    mkDeletePartnerEventSource,

    -- ** Request lenses
    dpesAccount,
    dpesName,

    -- * Destructuring the response
    DeletePartnerEventSourceResponse (..),
    mkDeletePartnerEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePartnerEventSource' smart constructor.
data DeletePartnerEventSource = DeletePartnerEventSource'
  { -- | The AWS account ID of the AWS customer that the event source was created for.
    account :: Lude.Text,
    -- | The name of the event source to delete.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePartnerEventSource' with the minimum fields required to make a request.
--
-- * 'account' - The AWS account ID of the AWS customer that the event source was created for.
-- * 'name' - The name of the event source to delete.
mkDeletePartnerEventSource ::
  -- | 'account'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  DeletePartnerEventSource
mkDeletePartnerEventSource pAccount_ pName_ =
  DeletePartnerEventSource' {account = pAccount_, name = pName_}

-- | The AWS account ID of the AWS customer that the event source was created for.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesAccount :: Lens.Lens' DeletePartnerEventSource Lude.Text
dpesAccount = Lens.lens (account :: DeletePartnerEventSource -> Lude.Text) (\s a -> s {account = a} :: DeletePartnerEventSource)
{-# DEPRECATED dpesAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The name of the event source to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesName :: Lens.Lens' DeletePartnerEventSource Lude.Text
dpesName = Lens.lens (name :: DeletePartnerEventSource -> Lude.Text) (\s a -> s {name = a} :: DeletePartnerEventSource)
{-# DEPRECATED dpesName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeletePartnerEventSource where
  type Rs DeletePartnerEventSource = DeletePartnerEventSourceResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull DeletePartnerEventSourceResponse'

instance Lude.ToHeaders DeletePartnerEventSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DeletePartnerEventSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePartnerEventSource where
  toJSON DeletePartnerEventSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Account" Lude..= account),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DeletePartnerEventSource where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePartnerEventSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePartnerEventSourceResponse' smart constructor.
data DeletePartnerEventSourceResponse = DeletePartnerEventSourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePartnerEventSourceResponse' with the minimum fields required to make a request.
mkDeletePartnerEventSourceResponse ::
  DeletePartnerEventSourceResponse
mkDeletePartnerEventSourceResponse =
  DeletePartnerEventSourceResponse'
