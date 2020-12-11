{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListAvailableZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Lists the Availability Zones that have available AWS CloudHSM capacity.
module Network.AWS.CloudHSM.ListAvailableZones
  ( -- * Creating a request
    ListAvailableZones (..),
    mkListAvailableZones,

    -- * Destructuring the response
    ListAvailableZonesResponse (..),
    mkListAvailableZonesResponse,

    -- ** Response lenses
    lazrsAZList,
    lazrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'ListAvailableZones' action.
--
-- /See:/ 'mkListAvailableZones' smart constructor.
data ListAvailableZones = ListAvailableZones'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAvailableZones' with the minimum fields required to make a request.
mkListAvailableZones ::
  ListAvailableZones
mkListAvailableZones = ListAvailableZones'

instance Lude.AWSRequest ListAvailableZones where
  type Rs ListAvailableZones = ListAvailableZonesResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAvailableZonesResponse'
            Lude.<$> (x Lude..?> "AZList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAvailableZones where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.ListAvailableZones" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAvailableZones where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ListAvailableZones where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAvailableZones where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAvailableZonesResponse' smart constructor.
data ListAvailableZonesResponse = ListAvailableZonesResponse'
  { aZList ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListAvailableZonesResponse' with the minimum fields required to make a request.
--
-- * 'aZList' - The list of Availability Zones that have available AWS CloudHSM capacity.
-- * 'responseStatus' - The response status code.
mkListAvailableZonesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAvailableZonesResponse
mkListAvailableZonesResponse pResponseStatus_ =
  ListAvailableZonesResponse'
    { aZList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of Availability Zones that have available AWS CloudHSM capacity.
--
-- /Note:/ Consider using 'aZList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lazrsAZList :: Lens.Lens' ListAvailableZonesResponse (Lude.Maybe [Lude.Text])
lazrsAZList = Lens.lens (aZList :: ListAvailableZonesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {aZList = a} :: ListAvailableZonesResponse)
{-# DEPRECATED lazrsAZList "Use generic-lens or generic-optics with 'aZList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lazrsResponseStatus :: Lens.Lens' ListAvailableZonesResponse Lude.Int
lazrsResponseStatus = Lens.lens (responseStatus :: ListAvailableZonesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAvailableZonesResponse)
{-# DEPRECATED lazrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
