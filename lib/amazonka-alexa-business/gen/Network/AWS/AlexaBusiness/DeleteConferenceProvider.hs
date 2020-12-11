{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conference provider.
module Network.AWS.AlexaBusiness.DeleteConferenceProvider
  ( -- * Creating a request
    DeleteConferenceProvider (..),
    mkDeleteConferenceProvider,

    -- ** Request lenses
    dcpConferenceProviderARN,

    -- * Destructuring the response
    DeleteConferenceProviderResponse (..),
    mkDeleteConferenceProviderResponse,

    -- ** Response lenses
    dcprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteConferenceProvider' smart constructor.
newtype DeleteConferenceProvider = DeleteConferenceProvider'
  { conferenceProviderARN ::
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

-- | Creates a value of 'DeleteConferenceProvider' with the minimum fields required to make a request.
--
-- * 'conferenceProviderARN' - The ARN of the conference provider.
mkDeleteConferenceProvider ::
  -- | 'conferenceProviderARN'
  Lude.Text ->
  DeleteConferenceProvider
mkDeleteConferenceProvider pConferenceProviderARN_ =
  DeleteConferenceProvider'
    { conferenceProviderARN =
        pConferenceProviderARN_
    }

-- | The ARN of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConferenceProviderARN :: Lens.Lens' DeleteConferenceProvider Lude.Text
dcpConferenceProviderARN = Lens.lens (conferenceProviderARN :: DeleteConferenceProvider -> Lude.Text) (\s a -> s {conferenceProviderARN = a} :: DeleteConferenceProvider)
{-# DEPRECATED dcpConferenceProviderARN "Use generic-lens or generic-optics with 'conferenceProviderARN' instead." #-}

instance Lude.AWSRequest DeleteConferenceProvider where
  type Rs DeleteConferenceProvider = DeleteConferenceProviderResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteConferenceProviderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConferenceProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteConferenceProvider" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConferenceProvider where
  toJSON DeleteConferenceProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ConferenceProviderArn" Lude..= conferenceProviderARN)
          ]
      )

instance Lude.ToPath DeleteConferenceProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConferenceProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConferenceProviderResponse' smart constructor.
newtype DeleteConferenceProviderResponse = DeleteConferenceProviderResponse'
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

-- | Creates a value of 'DeleteConferenceProviderResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConferenceProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConferenceProviderResponse
mkDeleteConferenceProviderResponse pResponseStatus_ =
  DeleteConferenceProviderResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DeleteConferenceProviderResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DeleteConferenceProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConferenceProviderResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
