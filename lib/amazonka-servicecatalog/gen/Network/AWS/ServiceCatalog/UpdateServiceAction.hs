{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a self-service action.
module Network.AWS.ServiceCatalog.UpdateServiceAction
  ( -- * Creating a request
    UpdateServiceAction (..),
    mkUpdateServiceAction,

    -- ** Request lenses
    usaDefinition,
    usaName,
    usaAcceptLanguage,
    usaId,
    usaDescription,

    -- * Destructuring the response
    UpdateServiceActionResponse (..),
    mkUpdateServiceActionResponse,

    -- ** Response lenses
    usarsServiceActionDetail,
    usarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateServiceAction' smart constructor.
data UpdateServiceAction = UpdateServiceAction'
  { -- | A map that defines the self-service action.
    definition :: Lude.Maybe (Lude.HashMap ServiceActionDefinitionKey (Lude.Text)),
    -- | The self-service action name.
    name :: Lude.Maybe Lude.Text,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The self-service action identifier.
    id :: Lude.Text,
    -- | The self-service action description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceAction' with the minimum fields required to make a request.
--
-- * 'definition' - A map that defines the self-service action.
-- * 'name' - The self-service action name.
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'id' - The self-service action identifier.
-- * 'description' - The self-service action description.
mkUpdateServiceAction ::
  -- | 'id'
  Lude.Text ->
  UpdateServiceAction
mkUpdateServiceAction pId_ =
  UpdateServiceAction'
    { definition = Lude.Nothing,
      name = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      id = pId_,
      description = Lude.Nothing
    }

-- | A map that defines the self-service action.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDefinition :: Lens.Lens' UpdateServiceAction (Lude.Maybe (Lude.HashMap ServiceActionDefinitionKey (Lude.Text)))
usaDefinition = Lens.lens (definition :: UpdateServiceAction -> Lude.Maybe (Lude.HashMap ServiceActionDefinitionKey (Lude.Text))) (\s a -> s {definition = a} :: UpdateServiceAction)
{-# DEPRECATED usaDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The self-service action name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaName :: Lens.Lens' UpdateServiceAction (Lude.Maybe Lude.Text)
usaName = Lens.lens (name :: UpdateServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateServiceAction)
{-# DEPRECATED usaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaAcceptLanguage :: Lens.Lens' UpdateServiceAction (Lude.Maybe Lude.Text)
usaAcceptLanguage = Lens.lens (acceptLanguage :: UpdateServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdateServiceAction)
{-# DEPRECATED usaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaId :: Lens.Lens' UpdateServiceAction Lude.Text
usaId = Lens.lens (id :: UpdateServiceAction -> Lude.Text) (\s a -> s {id = a} :: UpdateServiceAction)
{-# DEPRECATED usaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The self-service action description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDescription :: Lens.Lens' UpdateServiceAction (Lude.Maybe Lude.Text)
usaDescription = Lens.lens (description :: UpdateServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateServiceAction)
{-# DEPRECATED usaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateServiceAction where
  type Rs UpdateServiceAction = UpdateServiceActionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateServiceActionResponse'
            Lude.<$> (x Lude..?> "ServiceActionDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateServiceAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.UpdateServiceAction" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateServiceAction where
  toJSON UpdateServiceAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Definition" Lude..=) Lude.<$> definition,
            ("Name" Lude..=) Lude.<$> name,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateServiceAction where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServiceAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateServiceActionResponse' smart constructor.
data UpdateServiceActionResponse = UpdateServiceActionResponse'
  { -- | Detailed information about the self-service action.
    serviceActionDetail :: Lude.Maybe ServiceActionDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceActionResponse' with the minimum fields required to make a request.
--
-- * 'serviceActionDetail' - Detailed information about the self-service action.
-- * 'responseStatus' - The response status code.
mkUpdateServiceActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateServiceActionResponse
mkUpdateServiceActionResponse pResponseStatus_ =
  UpdateServiceActionResponse'
    { serviceActionDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarsServiceActionDetail :: Lens.Lens' UpdateServiceActionResponse (Lude.Maybe ServiceActionDetail)
usarsServiceActionDetail = Lens.lens (serviceActionDetail :: UpdateServiceActionResponse -> Lude.Maybe ServiceActionDetail) (\s a -> s {serviceActionDetail = a} :: UpdateServiceActionResponse)
{-# DEPRECATED usarsServiceActionDetail "Use generic-lens or generic-optics with 'serviceActionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarsResponseStatus :: Lens.Lens' UpdateServiceActionResponse Lude.Int
usarsResponseStatus = Lens.lens (responseStatus :: UpdateServiceActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServiceActionResponse)
{-# DEPRECATED usarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
