{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.PhoneNumberValidate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a phone number.
module Network.AWS.Pinpoint.PhoneNumberValidate
  ( -- * Creating a request
    PhoneNumberValidate (..),
    mkPhoneNumberValidate,

    -- ** Request lenses
    pnvNumberValidateRequest,

    -- * Destructuring the response
    PhoneNumberValidateResponse (..),
    mkPhoneNumberValidateResponse,

    -- ** Response lenses
    pnvrsResponseStatus,
    pnvrsNumberValidateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPhoneNumberValidate' smart constructor.
newtype PhoneNumberValidate = PhoneNumberValidate'
  { numberValidateRequest ::
      NumberValidateRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PhoneNumberValidate' with the minimum fields required to make a request.
--
-- * 'numberValidateRequest' - Undocumented field.
mkPhoneNumberValidate ::
  -- | 'numberValidateRequest'
  NumberValidateRequest ->
  PhoneNumberValidate
mkPhoneNumberValidate pNumberValidateRequest_ =
  PhoneNumberValidate'
    { numberValidateRequest =
        pNumberValidateRequest_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'numberValidateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvNumberValidateRequest :: Lens.Lens' PhoneNumberValidate NumberValidateRequest
pnvNumberValidateRequest = Lens.lens (numberValidateRequest :: PhoneNumberValidate -> NumberValidateRequest) (\s a -> s {numberValidateRequest = a} :: PhoneNumberValidate)
{-# DEPRECATED pnvNumberValidateRequest "Use generic-lens or generic-optics with 'numberValidateRequest' instead." #-}

instance Lude.AWSRequest PhoneNumberValidate where
  type Rs PhoneNumberValidate = PhoneNumberValidateResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          PhoneNumberValidateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders PhoneNumberValidate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PhoneNumberValidate where
  toJSON PhoneNumberValidate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("NumberValidateRequest" Lude..= numberValidateRequest)
          ]
      )

instance Lude.ToPath PhoneNumberValidate where
  toPath = Lude.const "/v1/phone/number/validate"

instance Lude.ToQuery PhoneNumberValidate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPhoneNumberValidateResponse' smart constructor.
data PhoneNumberValidateResponse = PhoneNumberValidateResponse'
  { responseStatus ::
      Lude.Int,
    numberValidateResponse ::
      NumberValidateResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PhoneNumberValidateResponse' with the minimum fields required to make a request.
--
-- * 'numberValidateResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPhoneNumberValidateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'numberValidateResponse'
  NumberValidateResponse ->
  PhoneNumberValidateResponse
mkPhoneNumberValidateResponse
  pResponseStatus_
  pNumberValidateResponse_ =
    PhoneNumberValidateResponse'
      { responseStatus = pResponseStatus_,
        numberValidateResponse = pNumberValidateResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvrsResponseStatus :: Lens.Lens' PhoneNumberValidateResponse Lude.Int
pnvrsResponseStatus = Lens.lens (responseStatus :: PhoneNumberValidateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PhoneNumberValidateResponse)
{-# DEPRECATED pnvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'numberValidateResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvrsNumberValidateResponse :: Lens.Lens' PhoneNumberValidateResponse NumberValidateResponse
pnvrsNumberValidateResponse = Lens.lens (numberValidateResponse :: PhoneNumberValidateResponse -> NumberValidateResponse) (\s a -> s {numberValidateResponse = a} :: PhoneNumberValidateResponse)
{-# DEPRECATED pnvrsNumberValidateResponse "Use generic-lens or generic-optics with 'numberValidateResponse' instead." #-}
