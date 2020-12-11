{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateContactAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the contact attributes associated with the specified contact.
--
-- You can add or update attributes for both ongoing and completed contacts. For example, you can update the customer's name or the reason the customer called while the call is active, or add notes about steps that the agent took during the call that are displayed to the next agent that takes the call. You can also update attributes for a contact using data from your CRM application and save the data with the contact in Amazon Connect. You could also flag calls for additional analysis, such as legal review or identifying abusive callers.
-- Contact attributes are available in Amazon Connect for 24 months, and are then deleted.
-- __Important:__ You cannot use the operation to update attributes for contacts that occurred prior to the release of the API, September 12, 2018. You can update attributes only for contacts that started after the release of the API. If you attempt to update attributes for a contact that occurred prior to the release of the API, a 400 error is returned. This applies also to queued callbacks that were initiated prior to the release of the API but are still active in your instance.
module Network.AWS.Connect.UpdateContactAttributes
  ( -- * Creating a request
    UpdateContactAttributes (..),
    mkUpdateContactAttributes,

    -- ** Request lenses
    ucaInitialContactId,
    ucaInstanceId,
    ucaAttributes,

    -- * Destructuring the response
    UpdateContactAttributesResponse (..),
    mkUpdateContactAttributesResponse,

    -- ** Response lenses
    ucarsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContactAttributes' smart constructor.
data UpdateContactAttributes = UpdateContactAttributes'
  { initialContactId ::
      Lude.Text,
    instanceId :: Lude.Text,
    attributes ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - The Amazon Connect attributes. These attributes can be accessed in contact flows just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
-- * 'initialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkUpdateContactAttributes ::
  -- | 'initialContactId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  UpdateContactAttributes
mkUpdateContactAttributes pInitialContactId_ pInstanceId_ =
  UpdateContactAttributes'
    { initialContactId = pInitialContactId_,
      instanceId = pInstanceId_,
      attributes = Lude.mempty
    }

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaInitialContactId :: Lens.Lens' UpdateContactAttributes Lude.Text
ucaInitialContactId = Lens.lens (initialContactId :: UpdateContactAttributes -> Lude.Text) (\s a -> s {initialContactId = a} :: UpdateContactAttributes)
{-# DEPRECATED ucaInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaInstanceId :: Lens.Lens' UpdateContactAttributes Lude.Text
ucaInstanceId = Lens.lens (instanceId :: UpdateContactAttributes -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateContactAttributes)
{-# DEPRECATED ucaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Connect attributes. These attributes can be accessed in contact flows just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaAttributes :: Lens.Lens' UpdateContactAttributes (Lude.HashMap Lude.Text (Lude.Text))
ucaAttributes = Lens.lens (attributes :: UpdateContactAttributes -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {attributes = a} :: UpdateContactAttributes)
{-# DEPRECATED ucaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest UpdateContactAttributes where
  type Rs UpdateContactAttributes = UpdateContactAttributesResponse
  request = Req.postJSON connectService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateContactAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContactAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContactAttributes where
  toJSON UpdateContactAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InitialContactId" Lude..= initialContactId),
            Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("Attributes" Lude..= attributes)
          ]
      )

instance Lude.ToPath UpdateContactAttributes where
  toPath = Lude.const "/contact/attributes"

instance Lude.ToQuery UpdateContactAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContactAttributesResponse' smart constructor.
newtype UpdateContactAttributesResponse = UpdateContactAttributesResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateContactAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContactAttributesResponse
mkUpdateContactAttributesResponse pResponseStatus_ =
  UpdateContactAttributesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarsResponseStatus :: Lens.Lens' UpdateContactAttributesResponse Lude.Int
ucarsResponseStatus = Lens.lens (responseStatus :: UpdateContactAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContactAttributesResponse)
{-# DEPRECATED ucarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
