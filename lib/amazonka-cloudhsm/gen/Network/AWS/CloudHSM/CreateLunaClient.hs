{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Creates an HSM client.
module Network.AWS.CloudHSM.CreateLunaClient
  ( -- * Creating a request
    CreateLunaClient (..),
    mkCreateLunaClient,

    -- ** Request lenses
    clcLabel,
    clcCertificate,

    -- * Destructuring the response
    CreateLunaClientResponse (..),
    mkCreateLunaClientResponse,

    -- ** Response lenses
    clcrsClientARN,
    clcrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'CreateLunaClient' action.
--
-- /See:/ 'mkCreateLunaClient' smart constructor.
data CreateLunaClient = CreateLunaClient'
  { label ::
      Lude.Maybe Lude.Text,
    certificate :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLunaClient' with the minimum fields required to make a request.
--
-- * 'certificate' - The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
-- * 'label' - The label for the client.
mkCreateLunaClient ::
  -- | 'certificate'
  Lude.Text ->
  CreateLunaClient
mkCreateLunaClient pCertificate_ =
  CreateLunaClient'
    { label = Lude.Nothing,
      certificate = pCertificate_
    }

-- | The label for the client.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcLabel :: Lens.Lens' CreateLunaClient (Lude.Maybe Lude.Text)
clcLabel = Lens.lens (label :: CreateLunaClient -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: CreateLunaClient)
{-# DEPRECATED clcLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcCertificate :: Lens.Lens' CreateLunaClient Lude.Text
clcCertificate = Lens.lens (certificate :: CreateLunaClient -> Lude.Text) (\s a -> s {certificate = a} :: CreateLunaClient)
{-# DEPRECATED clcCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

instance Lude.AWSRequest CreateLunaClient where
  type Rs CreateLunaClient = CreateLunaClientResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLunaClientResponse'
            Lude.<$> (x Lude..?> "ClientArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLunaClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.CreateLunaClient" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLunaClient where
  toJSON CreateLunaClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Label" Lude..=) Lude.<$> label,
            Lude.Just ("Certificate" Lude..= certificate)
          ]
      )

instance Lude.ToPath CreateLunaClient where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLunaClient where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'CreateLunaClient' action.
--
-- /See:/ 'mkCreateLunaClientResponse' smart constructor.
data CreateLunaClientResponse = CreateLunaClientResponse'
  { clientARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateLunaClientResponse' with the minimum fields required to make a request.
--
-- * 'clientARN' - The ARN of the client.
-- * 'responseStatus' - The response status code.
mkCreateLunaClientResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLunaClientResponse
mkCreateLunaClientResponse pResponseStatus_ =
  CreateLunaClientResponse'
    { clientARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcrsClientARN :: Lens.Lens' CreateLunaClientResponse (Lude.Maybe Lude.Text)
clcrsClientARN = Lens.lens (clientARN :: CreateLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientARN = a} :: CreateLunaClientResponse)
{-# DEPRECATED clcrsClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcrsResponseStatus :: Lens.Lens' CreateLunaClientResponse Lude.Int
clcrsResponseStatus = Lens.lens (responseStatus :: CreateLunaClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLunaClientResponse)
{-# DEPRECATED clcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
