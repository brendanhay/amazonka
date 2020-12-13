{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates one or more configuration items from an application.
module Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
  ( -- * Creating a request
    DisassociateConfigurationItemsFromApplication (..),
    mkDisassociateConfigurationItemsFromApplication,

    -- ** Request lenses
    dcifaApplicationConfigurationId,
    dcifaConfigurationIds,

    -- * Destructuring the response
    DisassociateConfigurationItemsFromApplicationResponse (..),
    mkDisassociateConfigurationItemsFromApplicationResponse,

    -- ** Response lenses
    dcifarsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateConfigurationItemsFromApplication' smart constructor.
data DisassociateConfigurationItemsFromApplication = DisassociateConfigurationItemsFromApplication'
  { -- | Configuration ID of an application from which each item is disassociated.
    applicationConfigurationId :: Lude.Text,
    -- | Configuration ID of each item to be disassociated from an application.
    configurationIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateConfigurationItemsFromApplication' with the minimum fields required to make a request.
--
-- * 'applicationConfigurationId' - Configuration ID of an application from which each item is disassociated.
-- * 'configurationIds' - Configuration ID of each item to be disassociated from an application.
mkDisassociateConfigurationItemsFromApplication ::
  -- | 'applicationConfigurationId'
  Lude.Text ->
  DisassociateConfigurationItemsFromApplication
mkDisassociateConfigurationItemsFromApplication
  pApplicationConfigurationId_ =
    DisassociateConfigurationItemsFromApplication'
      { applicationConfigurationId =
          pApplicationConfigurationId_,
        configurationIds = Lude.mempty
      }

-- | Configuration ID of an application from which each item is disassociated.
--
-- /Note:/ Consider using 'applicationConfigurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifaApplicationConfigurationId :: Lens.Lens' DisassociateConfigurationItemsFromApplication Lude.Text
dcifaApplicationConfigurationId = Lens.lens (applicationConfigurationId :: DisassociateConfigurationItemsFromApplication -> Lude.Text) (\s a -> s {applicationConfigurationId = a} :: DisassociateConfigurationItemsFromApplication)
{-# DEPRECATED dcifaApplicationConfigurationId "Use generic-lens or generic-optics with 'applicationConfigurationId' instead." #-}

-- | Configuration ID of each item to be disassociated from an application.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifaConfigurationIds :: Lens.Lens' DisassociateConfigurationItemsFromApplication [Lude.Text]
dcifaConfigurationIds = Lens.lens (configurationIds :: DisassociateConfigurationItemsFromApplication -> [Lude.Text]) (\s a -> s {configurationIds = a} :: DisassociateConfigurationItemsFromApplication)
{-# DEPRECATED dcifaConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance
  Lude.AWSRequest
    DisassociateConfigurationItemsFromApplication
  where
  type
    Rs DisassociateConfigurationItemsFromApplication =
      DisassociateConfigurationItemsFromApplicationResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateConfigurationItemsFromApplicationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DisassociateConfigurationItemsFromApplication
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DisassociateConfigurationItemsFromApplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateConfigurationItemsFromApplication where
  toJSON DisassociateConfigurationItemsFromApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("applicationConfigurationId" Lude..= applicationConfigurationId),
            Lude.Just ("configurationIds" Lude..= configurationIds)
          ]
      )

instance Lude.ToPath DisassociateConfigurationItemsFromApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateConfigurationItemsFromApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateConfigurationItemsFromApplicationResponse' smart constructor.
newtype DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateConfigurationItemsFromApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateConfigurationItemsFromApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateConfigurationItemsFromApplicationResponse
mkDisassociateConfigurationItemsFromApplicationResponse
  pResponseStatus_ =
    DisassociateConfigurationItemsFromApplicationResponse'
      { responseStatus =
          pResponseStatus_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifarsResponseStatus :: Lens.Lens' DisassociateConfigurationItemsFromApplicationResponse Lude.Int
dcifarsResponseStatus = Lens.lens (responseStatus :: DisassociateConfigurationItemsFromApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateConfigurationItemsFromApplicationResponse)
{-# DEPRECATED dcifarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
