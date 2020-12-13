{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetRegistrationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a registration code used to register a CA certificate with AWS IoT.
module Network.AWS.IoT.GetRegistrationCode
  ( -- * Creating a request
    GetRegistrationCode (..),
    mkGetRegistrationCode,

    -- * Destructuring the response
    GetRegistrationCodeResponse (..),
    mkGetRegistrationCodeResponse,

    -- ** Response lenses
    grcrsRegistrationCode,
    grcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the GetRegistrationCode operation.
--
-- /See:/ 'mkGetRegistrationCode' smart constructor.
data GetRegistrationCode = GetRegistrationCode'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRegistrationCode' with the minimum fields required to make a request.
mkGetRegistrationCode ::
  GetRegistrationCode
mkGetRegistrationCode = GetRegistrationCode'

instance Lude.AWSRequest GetRegistrationCode where
  type Rs GetRegistrationCode = GetRegistrationCodeResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRegistrationCodeResponse'
            Lude.<$> (x Lude..?> "registrationCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRegistrationCode where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetRegistrationCode where
  toPath = Lude.const "/registrationcode"

instance Lude.ToQuery GetRegistrationCode where
  toQuery = Lude.const Lude.mempty

-- | The output from the GetRegistrationCode operation.
--
-- /See:/ 'mkGetRegistrationCodeResponse' smart constructor.
data GetRegistrationCodeResponse = GetRegistrationCodeResponse'
  { -- | The CA certificate registration code.
    registrationCode :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRegistrationCodeResponse' with the minimum fields required to make a request.
--
-- * 'registrationCode' - The CA certificate registration code.
-- * 'responseStatus' - The response status code.
mkGetRegistrationCodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRegistrationCodeResponse
mkGetRegistrationCodeResponse pResponseStatus_ =
  GetRegistrationCodeResponse'
    { registrationCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The CA certificate registration code.
--
-- /Note:/ Consider using 'registrationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsRegistrationCode :: Lens.Lens' GetRegistrationCodeResponse (Lude.Maybe Lude.Text)
grcrsRegistrationCode = Lens.lens (registrationCode :: GetRegistrationCodeResponse -> Lude.Maybe Lude.Text) (\s a -> s {registrationCode = a} :: GetRegistrationCodeResponse)
{-# DEPRECATED grcrsRegistrationCode "Use generic-lens or generic-optics with 'registrationCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsResponseStatus :: Lens.Lens' GetRegistrationCodeResponse Lude.Int
grcrsResponseStatus = Lens.lens (responseStatus :: GetRegistrationCodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRegistrationCodeResponse)
{-# DEPRECATED grcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
