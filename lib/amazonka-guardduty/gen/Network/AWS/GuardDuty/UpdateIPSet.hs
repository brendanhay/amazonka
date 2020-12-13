{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the IPSet specified by the IPSet ID.
module Network.AWS.GuardDuty.UpdateIPSet
  ( -- * Creating a request
    UpdateIPSet (..),
    mkUpdateIPSet,

    -- ** Request lenses
    uisLocation,
    uisActivate,
    uisDetectorId,
    uisName,
    uisIPSetId,

    -- * Destructuring the response
    UpdateIPSetResponse (..),
    mkUpdateIPSetResponse,

    -- ** Response lenses
    uisrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { -- | The updated URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
    location :: Lude.Maybe Lude.Text,
    -- | The updated Boolean value that specifies whether the IPSet is active or not.
    activate :: Lude.Maybe Lude.Bool,
    -- | The detectorID that specifies the GuardDuty service whose IPSet you want to update.
    detectorId :: Lude.Text,
    -- | The unique ID that specifies the IPSet that you want to update.
    name :: Lude.Maybe Lude.Text,
    -- | The unique ID that specifies the IPSet that you want to update.
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIPSet' with the minimum fields required to make a request.
--
-- * 'location' - The updated URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
-- * 'activate' - The updated Boolean value that specifies whether the IPSet is active or not.
-- * 'detectorId' - The detectorID that specifies the GuardDuty service whose IPSet you want to update.
-- * 'name' - The unique ID that specifies the IPSet that you want to update.
-- * 'ipSetId' - The unique ID that specifies the IPSet that you want to update.
mkUpdateIPSet ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'ipSetId'
  Lude.Text ->
  UpdateIPSet
mkUpdateIPSet pDetectorId_ pIPSetId_ =
  UpdateIPSet'
    { location = Lude.Nothing,
      activate = Lude.Nothing,
      detectorId = pDetectorId_,
      name = Lude.Nothing,
      ipSetId = pIPSetId_
    }

-- | The updated URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisLocation :: Lens.Lens' UpdateIPSet (Lude.Maybe Lude.Text)
uisLocation = Lens.lens (location :: UpdateIPSet -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: UpdateIPSet)
{-# DEPRECATED uisLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The updated Boolean value that specifies whether the IPSet is active or not.
--
-- /Note:/ Consider using 'activate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisActivate :: Lens.Lens' UpdateIPSet (Lude.Maybe Lude.Bool)
uisActivate = Lens.lens (activate :: UpdateIPSet -> Lude.Maybe Lude.Bool) (\s a -> s {activate = a} :: UpdateIPSet)
{-# DEPRECATED uisActivate "Use generic-lens or generic-optics with 'activate' instead." #-}

-- | The detectorID that specifies the GuardDuty service whose IPSet you want to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisDetectorId :: Lens.Lens' UpdateIPSet Lude.Text
uisDetectorId = Lens.lens (detectorId :: UpdateIPSet -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateIPSet)
{-# DEPRECATED uisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID that specifies the IPSet that you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisName :: Lens.Lens' UpdateIPSet (Lude.Maybe Lude.Text)
uisName = Lens.lens (name :: UpdateIPSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateIPSet)
{-# DEPRECATED uisName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique ID that specifies the IPSet that you want to update.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisIPSetId :: Lens.Lens' UpdateIPSet Lude.Text
uisIPSetId = Lens.lens (ipSetId :: UpdateIPSet -> Lude.Text) (\s a -> s {ipSetId = a} :: UpdateIPSet)
{-# DEPRECATED uisIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.AWSRequest UpdateIPSet where
  type Rs UpdateIPSet = UpdateIPSetResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateIPSetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateIPSet where
  toJSON UpdateIPSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("location" Lude..=) Lude.<$> location,
            ("activate" Lude..=) Lude.<$> activate,
            ("name" Lude..=) Lude.<$> name
          ]
      )

instance Lude.ToPath UpdateIPSet where
  toPath UpdateIPSet' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/ipset/", Lude.toBS ipSetId]

instance Lude.ToQuery UpdateIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateIPSetResponse' smart constructor.
newtype UpdateIPSetResponse = UpdateIPSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIPSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateIPSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateIPSetResponse
mkUpdateIPSetResponse pResponseStatus_ =
  UpdateIPSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisrsResponseStatus :: Lens.Lens' UpdateIPSetResponse Lude.Int
uisrsResponseStatus = Lens.lens (responseStatus :: UpdateIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateIPSetResponse)
{-# DEPRECATED uisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
