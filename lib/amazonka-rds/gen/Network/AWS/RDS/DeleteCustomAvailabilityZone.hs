{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteCustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom Availability Zone (AZ).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
module Network.AWS.RDS.DeleteCustomAvailabilityZone
  ( -- * Creating a request
    DeleteCustomAvailabilityZone (..),
    mkDeleteCustomAvailabilityZone,

    -- ** Request lenses
    dcazCustomAvailabilityZoneId,

    -- * Destructuring the response
    DeleteCustomAvailabilityZoneResponse (..),
    mkDeleteCustomAvailabilityZoneResponse,

    -- ** Response lenses
    dcazrsCustomAvailabilityZone,
    dcazrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCustomAvailabilityZone' smart constructor.
newtype DeleteCustomAvailabilityZone = DeleteCustomAvailabilityZone'
  { -- | The custom AZ identifier.
    customAvailabilityZoneId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomAvailabilityZone' with the minimum fields required to make a request.
--
-- * 'customAvailabilityZoneId' - The custom AZ identifier.
mkDeleteCustomAvailabilityZone ::
  -- | 'customAvailabilityZoneId'
  Lude.Text ->
  DeleteCustomAvailabilityZone
mkDeleteCustomAvailabilityZone pCustomAvailabilityZoneId_ =
  DeleteCustomAvailabilityZone'
    { customAvailabilityZoneId =
        pCustomAvailabilityZoneId_
    }

-- | The custom AZ identifier.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazCustomAvailabilityZoneId :: Lens.Lens' DeleteCustomAvailabilityZone Lude.Text
dcazCustomAvailabilityZoneId = Lens.lens (customAvailabilityZoneId :: DeleteCustomAvailabilityZone -> Lude.Text) (\s a -> s {customAvailabilityZoneId = a} :: DeleteCustomAvailabilityZone)
{-# DEPRECATED dcazCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

instance Lude.AWSRequest DeleteCustomAvailabilityZone where
  type
    Rs DeleteCustomAvailabilityZone =
      DeleteCustomAvailabilityZoneResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteCustomAvailabilityZoneResult"
      ( \s h x ->
          DeleteCustomAvailabilityZoneResponse'
            Lude.<$> (x Lude..@? "CustomAvailabilityZone")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCustomAvailabilityZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCustomAvailabilityZone where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCustomAvailabilityZone where
  toQuery DeleteCustomAvailabilityZone' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteCustomAvailabilityZone" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "CustomAvailabilityZoneId" Lude.=: customAvailabilityZoneId
      ]

-- | /See:/ 'mkDeleteCustomAvailabilityZoneResponse' smart constructor.
data DeleteCustomAvailabilityZoneResponse = DeleteCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Lude.Maybe CustomAvailabilityZone,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomAvailabilityZoneResponse' with the minimum fields required to make a request.
--
-- * 'customAvailabilityZone' -
-- * 'responseStatus' - The response status code.
mkDeleteCustomAvailabilityZoneResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCustomAvailabilityZoneResponse
mkDeleteCustomAvailabilityZoneResponse pResponseStatus_ =
  DeleteCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'customAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrsCustomAvailabilityZone :: Lens.Lens' DeleteCustomAvailabilityZoneResponse (Lude.Maybe CustomAvailabilityZone)
dcazrsCustomAvailabilityZone = Lens.lens (customAvailabilityZone :: DeleteCustomAvailabilityZoneResponse -> Lude.Maybe CustomAvailabilityZone) (\s a -> s {customAvailabilityZone = a} :: DeleteCustomAvailabilityZoneResponse)
{-# DEPRECATED dcazrsCustomAvailabilityZone "Use generic-lens or generic-optics with 'customAvailabilityZone' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrsResponseStatus :: Lens.Lens' DeleteCustomAvailabilityZoneResponse Lude.Int
dcazrsResponseStatus = Lens.lens (responseStatus :: DeleteCustomAvailabilityZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCustomAvailabilityZoneResponse)
{-# DEPRECATED dcazrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
