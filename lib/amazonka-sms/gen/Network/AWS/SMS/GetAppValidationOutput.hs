{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves output from validating an application.
module Network.AWS.SMS.GetAppValidationOutput
  ( -- * Creating a request
    GetAppValidationOutput (..),
    mkGetAppValidationOutput,

    -- ** Request lenses
    gavoAppId,

    -- * Destructuring the response
    GetAppValidationOutputResponse (..),
    mkGetAppValidationOutputResponse,

    -- ** Response lenses
    gavorsValidationOutputList,
    gavorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetAppValidationOutput' smart constructor.
newtype GetAppValidationOutput = GetAppValidationOutput'
  { appId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppValidationOutput' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkGetAppValidationOutput ::
  -- | 'appId'
  Lude.Text ->
  GetAppValidationOutput
mkGetAppValidationOutput pAppId_ =
  GetAppValidationOutput' {appId = pAppId_}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavoAppId :: Lens.Lens' GetAppValidationOutput Lude.Text
gavoAppId = Lens.lens (appId :: GetAppValidationOutput -> Lude.Text) (\s a -> s {appId = a} :: GetAppValidationOutput)
{-# DEPRECATED gavoAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest GetAppValidationOutput where
  type Rs GetAppValidationOutput = GetAppValidationOutputResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppValidationOutputResponse'
            Lude.<$> (x Lude..?> "validationOutputList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAppValidationOutput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetAppValidationOutput" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAppValidationOutput where
  toJSON GetAppValidationOutput' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("appId" Lude..= appId)])

instance Lude.ToPath GetAppValidationOutput where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAppValidationOutput where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppValidationOutputResponse' smart constructor.
data GetAppValidationOutputResponse = GetAppValidationOutputResponse'
  { validationOutputList ::
      Lude.Maybe [ValidationOutput],
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

-- | Creates a value of 'GetAppValidationOutputResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'validationOutputList' - The validation output.
mkGetAppValidationOutputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppValidationOutputResponse
mkGetAppValidationOutputResponse pResponseStatus_ =
  GetAppValidationOutputResponse'
    { validationOutputList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The validation output.
--
-- /Note:/ Consider using 'validationOutputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavorsValidationOutputList :: Lens.Lens' GetAppValidationOutputResponse (Lude.Maybe [ValidationOutput])
gavorsValidationOutputList = Lens.lens (validationOutputList :: GetAppValidationOutputResponse -> Lude.Maybe [ValidationOutput]) (\s a -> s {validationOutputList = a} :: GetAppValidationOutputResponse)
{-# DEPRECATED gavorsValidationOutputList "Use generic-lens or generic-optics with 'validationOutputList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavorsResponseStatus :: Lens.Lens' GetAppValidationOutputResponse Lude.Int
gavorsResponseStatus = Lens.lens (responseStatus :: GetAppValidationOutputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppValidationOutputResponse)
{-# DEPRECATED gavorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
