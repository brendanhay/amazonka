{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateThreatIntelSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.UpdateThreatIntelSet
  ( -- * Creating a request
    UpdateThreatIntelSet (..),
    mkUpdateThreatIntelSet,

    -- ** Request lenses
    utisLocation,
    utisThreatIntelSetId,
    utisActivate,
    utisDetectorId,
    utisName,

    -- * Destructuring the response
    UpdateThreatIntelSetResponse (..),
    mkUpdateThreatIntelSetResponse,

    -- ** Response lenses
    utisrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateThreatIntelSet' smart constructor.
data UpdateThreatIntelSet = UpdateThreatIntelSet'
  { -- | The updated URI of the file that contains the ThreateIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
    location :: Lude.Maybe Lude.Text,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    threatIntelSetId :: Lude.Text,
    -- | The updated Boolean value that specifies whether the ThreateIntelSet is active or not.
    activate :: Lude.Maybe Lude.Bool,
    -- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet you want to update.
    detectorId :: Lude.Text,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThreatIntelSet' with the minimum fields required to make a request.
--
-- * 'location' - The updated URI of the file that contains the ThreateIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
-- * 'threatIntelSetId' - The unique ID that specifies the ThreatIntelSet that you want to update.
-- * 'activate' - The updated Boolean value that specifies whether the ThreateIntelSet is active or not.
-- * 'detectorId' - The detectorID that specifies the GuardDuty service whose ThreatIntelSet you want to update.
-- * 'name' - The unique ID that specifies the ThreatIntelSet that you want to update.
mkUpdateThreatIntelSet ::
  -- | 'threatIntelSetId'
  Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  UpdateThreatIntelSet
mkUpdateThreatIntelSet pThreatIntelSetId_ pDetectorId_ =
  UpdateThreatIntelSet'
    { location = Lude.Nothing,
      threatIntelSetId = pThreatIntelSetId_,
      activate = Lude.Nothing,
      detectorId = pDetectorId_,
      name = Lude.Nothing
    }

-- | The updated URI of the file that contains the ThreateIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisLocation :: Lens.Lens' UpdateThreatIntelSet (Lude.Maybe Lude.Text)
utisLocation = Lens.lens (location :: UpdateThreatIntelSet -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: UpdateThreatIntelSet)
{-# DEPRECATED utisLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- /Note:/ Consider using 'threatIntelSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisThreatIntelSetId :: Lens.Lens' UpdateThreatIntelSet Lude.Text
utisThreatIntelSetId = Lens.lens (threatIntelSetId :: UpdateThreatIntelSet -> Lude.Text) (\s a -> s {threatIntelSetId = a} :: UpdateThreatIntelSet)
{-# DEPRECATED utisThreatIntelSetId "Use generic-lens or generic-optics with 'threatIntelSetId' instead." #-}

-- | The updated Boolean value that specifies whether the ThreateIntelSet is active or not.
--
-- /Note:/ Consider using 'activate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisActivate :: Lens.Lens' UpdateThreatIntelSet (Lude.Maybe Lude.Bool)
utisActivate = Lens.lens (activate :: UpdateThreatIntelSet -> Lude.Maybe Lude.Bool) (\s a -> s {activate = a} :: UpdateThreatIntelSet)
{-# DEPRECATED utisActivate "Use generic-lens or generic-optics with 'activate' instead." #-}

-- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet you want to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisDetectorId :: Lens.Lens' UpdateThreatIntelSet Lude.Text
utisDetectorId = Lens.lens (detectorId :: UpdateThreatIntelSet -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateThreatIntelSet)
{-# DEPRECATED utisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisName :: Lens.Lens' UpdateThreatIntelSet (Lude.Maybe Lude.Text)
utisName = Lens.lens (name :: UpdateThreatIntelSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateThreatIntelSet)
{-# DEPRECATED utisName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateThreatIntelSet where
  type Rs UpdateThreatIntelSet = UpdateThreatIntelSetResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateThreatIntelSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateThreatIntelSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateThreatIntelSet where
  toJSON UpdateThreatIntelSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("location" Lude..=) Lude.<$> location,
            ("activate" Lude..=) Lude.<$> activate,
            ("name" Lude..=) Lude.<$> name
          ]
      )

instance Lude.ToPath UpdateThreatIntelSet where
  toPath UpdateThreatIntelSet' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/threatintelset/",
        Lude.toBS threatIntelSetId
      ]

instance Lude.ToQuery UpdateThreatIntelSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateThreatIntelSetResponse' smart constructor.
newtype UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThreatIntelSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateThreatIntelSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateThreatIntelSetResponse
mkUpdateThreatIntelSetResponse pResponseStatus_ =
  UpdateThreatIntelSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisrsResponseStatus :: Lens.Lens' UpdateThreatIntelSetResponse Lude.Int
utisrsResponseStatus = Lens.lens (responseStatus :: UpdateThreatIntelSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateThreatIntelSetResponse)
{-# DEPRECATED utisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
