{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.AssociateConfigurationItemsToApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more configuration items with an application.
module Network.AWS.Discovery.AssociateConfigurationItemsToApplication
  ( -- * Creating a request
    AssociateConfigurationItemsToApplication (..),
    mkAssociateConfigurationItemsToApplication,

    -- ** Request lenses
    acitaApplicationConfigurationId,
    acitaConfigurationIds,

    -- * Destructuring the response
    AssociateConfigurationItemsToApplicationResponse (..),
    mkAssociateConfigurationItemsToApplicationResponse,

    -- ** Response lenses
    acitarsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateConfigurationItemsToApplication' smart constructor.
data AssociateConfigurationItemsToApplication = AssociateConfigurationItemsToApplication'
  { -- | The configuration ID of an application with which items are to be associated.
    applicationConfigurationId :: Lude.Text,
    -- | The ID of each configuration item to be associated with an application.
    configurationIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateConfigurationItemsToApplication' with the minimum fields required to make a request.
--
-- * 'applicationConfigurationId' - The configuration ID of an application with which items are to be associated.
-- * 'configurationIds' - The ID of each configuration item to be associated with an application.
mkAssociateConfigurationItemsToApplication ::
  -- | 'applicationConfigurationId'
  Lude.Text ->
  AssociateConfigurationItemsToApplication
mkAssociateConfigurationItemsToApplication
  pApplicationConfigurationId_ =
    AssociateConfigurationItemsToApplication'
      { applicationConfigurationId =
          pApplicationConfigurationId_,
        configurationIds = Lude.mempty
      }

-- | The configuration ID of an application with which items are to be associated.
--
-- /Note:/ Consider using 'applicationConfigurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitaApplicationConfigurationId :: Lens.Lens' AssociateConfigurationItemsToApplication Lude.Text
acitaApplicationConfigurationId = Lens.lens (applicationConfigurationId :: AssociateConfigurationItemsToApplication -> Lude.Text) (\s a -> s {applicationConfigurationId = a} :: AssociateConfigurationItemsToApplication)
{-# DEPRECATED acitaApplicationConfigurationId "Use generic-lens or generic-optics with 'applicationConfigurationId' instead." #-}

-- | The ID of each configuration item to be associated with an application.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitaConfigurationIds :: Lens.Lens' AssociateConfigurationItemsToApplication [Lude.Text]
acitaConfigurationIds = Lens.lens (configurationIds :: AssociateConfigurationItemsToApplication -> [Lude.Text]) (\s a -> s {configurationIds = a} :: AssociateConfigurationItemsToApplication)
{-# DEPRECATED acitaConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance Lude.AWSRequest AssociateConfigurationItemsToApplication where
  type
    Rs AssociateConfigurationItemsToApplication =
      AssociateConfigurationItemsToApplicationResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateConfigurationItemsToApplicationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateConfigurationItemsToApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.AssociateConfigurationItemsToApplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateConfigurationItemsToApplication where
  toJSON AssociateConfigurationItemsToApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("applicationConfigurationId" Lude..= applicationConfigurationId),
            Lude.Just ("configurationIds" Lude..= configurationIds)
          ]
      )

instance Lude.ToPath AssociateConfigurationItemsToApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateConfigurationItemsToApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateConfigurationItemsToApplicationResponse' smart constructor.
newtype AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateConfigurationItemsToApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateConfigurationItemsToApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateConfigurationItemsToApplicationResponse
mkAssociateConfigurationItemsToApplicationResponse pResponseStatus_ =
  AssociateConfigurationItemsToApplicationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitarsResponseStatus :: Lens.Lens' AssociateConfigurationItemsToApplicationResponse Lude.Int
acitarsResponseStatus = Lens.lens (responseStatus :: AssociateConfigurationItemsToApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateConfigurationItemsToApplicationResponse)
{-# DEPRECATED acitarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
