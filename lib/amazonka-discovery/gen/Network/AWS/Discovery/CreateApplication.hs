{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application with the given name and description.
module Network.AWS.Discovery.CreateApplication
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caName,
    caDescription,

    -- * Destructuring the response
    CreateApplicationResponse (..),
    mkCreateApplicationResponse,

    -- ** Response lenses
    carsConfigurationId,
    carsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | Name of the application to be created.
    name :: Lude.Text,
    -- | Description of the application to be created.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- * 'name' - Name of the application to be created.
-- * 'description' - Description of the application to be created.
mkCreateApplication ::
  -- | 'name'
  Lude.Text ->
  CreateApplication
mkCreateApplication pName_ =
  CreateApplication' {name = pName_, description = Lude.Nothing}

-- | Name of the application to be created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApplication Lude.Text
caName = Lens.lens (name :: CreateApplication -> Lude.Text) (\s a -> s {name = a} :: CreateApplication)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Description of the application to be created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateApplication)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateApplication where
  type Rs CreateApplication = CreateApplicationResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Lude.<$> (x Lude..?> "configurationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.CreateApplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | Configuration ID of an application to be created.
    configurationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- * 'configurationId' - Configuration ID of an application to be created.
-- * 'responseStatus' - The response status code.
mkCreateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateApplicationResponse
mkCreateApplicationResponse pResponseStatus_ =
  CreateApplicationResponse'
    { configurationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Configuration ID of an application to be created.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsConfigurationId :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsConfigurationId = Lens.lens (configurationId :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {configurationId = a} :: CreateApplicationResponse)
{-# DEPRECATED carsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateApplicationResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateApplicationResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
