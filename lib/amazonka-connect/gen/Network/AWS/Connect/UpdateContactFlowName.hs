{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateContactFlowName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The name of the contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.UpdateContactFlowName
  ( -- * Creating a request
    UpdateContactFlowName (..),
    mkUpdateContactFlowName,

    -- ** Request lenses
    ucfnInstanceId,
    ucfnContactFlowId,
    ucfnName,
    ucfnDescription,

    -- * Destructuring the response
    UpdateContactFlowNameResponse (..),
    mkUpdateContactFlowNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContactFlowName' smart constructor.
data UpdateContactFlowName = UpdateContactFlowName'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Lude.Text,
    -- | The name of the contact flow.
    name :: Lude.Maybe Lude.Text,
    -- | The description of the contact flow.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactFlowName' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'contactFlowId' - The identifier of the contact flow.
-- * 'name' - The name of the contact flow.
-- * 'description' - The description of the contact flow.
mkUpdateContactFlowName ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactFlowId'
  Lude.Text ->
  UpdateContactFlowName
mkUpdateContactFlowName pInstanceId_ pContactFlowId_ =
  UpdateContactFlowName'
    { instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfnInstanceId :: Lens.Lens' UpdateContactFlowName Lude.Text
ucfnInstanceId = Lens.lens (instanceId :: UpdateContactFlowName -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateContactFlowName)
{-# DEPRECATED ucfnInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfnContactFlowId :: Lens.Lens' UpdateContactFlowName Lude.Text
ucfnContactFlowId = Lens.lens (contactFlowId :: UpdateContactFlowName -> Lude.Text) (\s a -> s {contactFlowId = a} :: UpdateContactFlowName)
{-# DEPRECATED ucfnContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfnName :: Lens.Lens' UpdateContactFlowName (Lude.Maybe Lude.Text)
ucfnName = Lens.lens (name :: UpdateContactFlowName -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateContactFlowName)
{-# DEPRECATED ucfnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the contact flow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfnDescription :: Lens.Lens' UpdateContactFlowName (Lude.Maybe Lude.Text)
ucfnDescription = Lens.lens (description :: UpdateContactFlowName -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateContactFlowName)
{-# DEPRECATED ucfnDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateContactFlowName where
  type Rs UpdateContactFlowName = UpdateContactFlowNameResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateContactFlowNameResponse'

instance Lude.ToHeaders UpdateContactFlowName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContactFlowName where
  toJSON UpdateContactFlowName' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateContactFlowName where
  toPath UpdateContactFlowName' {..} =
    Lude.mconcat
      [ "/contact-flows/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS contactFlowId,
        "/name"
      ]

instance Lude.ToQuery UpdateContactFlowName where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContactFlowNameResponse' smart constructor.
data UpdateContactFlowNameResponse = UpdateContactFlowNameResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactFlowNameResponse' with the minimum fields required to make a request.
mkUpdateContactFlowNameResponse ::
  UpdateContactFlowNameResponse
mkUpdateContactFlowNameResponse = UpdateContactFlowNameResponse'
