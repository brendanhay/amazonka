{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Core
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Core
  ( Core (..),

    -- * Smart constructor
    mkCore,

    -- * Lenses
    cSyncShadow,
    cThingARN,
    cId,
    cCertificateARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a core.
--
-- /See:/ 'mkCore' smart constructor.
data Core = Core'
  { syncShadow :: Lude.Maybe Lude.Bool,
    thingARN :: Lude.Text,
    id :: Lude.Text,
    certificateARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Core' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the certificate associated with the core.
-- * 'id' - A descriptive or arbitrary ID for the core. This value must be unique within the core definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
-- * 'syncShadow' - If true, the core's local shadow is automatically synced with the cloud.
-- * 'thingARN' - The ARN of the thing which is the core.
mkCore ::
  -- | 'thingARN'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'certificateARN'
  Lude.Text ->
  Core
mkCore pThingARN_ pId_ pCertificateARN_ =
  Core'
    { syncShadow = Lude.Nothing,
      thingARN = pThingARN_,
      id = pId_,
      certificateARN = pCertificateARN_
    }

-- | If true, the core's local shadow is automatically synced with the cloud.
--
-- /Note:/ Consider using 'syncShadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSyncShadow :: Lens.Lens' Core (Lude.Maybe Lude.Bool)
cSyncShadow = Lens.lens (syncShadow :: Core -> Lude.Maybe Lude.Bool) (\s a -> s {syncShadow = a} :: Core)
{-# DEPRECATED cSyncShadow "Use generic-lens or generic-optics with 'syncShadow' instead." #-}

-- | The ARN of the thing which is the core.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThingARN :: Lens.Lens' Core Lude.Text
cThingARN = Lens.lens (thingARN :: Core -> Lude.Text) (\s a -> s {thingARN = a} :: Core)
{-# DEPRECATED cThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | A descriptive or arbitrary ID for the core. This value must be unique within the core definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Core Lude.Text
cId = Lens.lens (id :: Core -> Lude.Text) (\s a -> s {id = a} :: Core)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN of the certificate associated with the core.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateARN :: Lens.Lens' Core Lude.Text
cCertificateARN = Lens.lens (certificateARN :: Core -> Lude.Text) (\s a -> s {certificateARN = a} :: Core)
{-# DEPRECATED cCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.FromJSON Core where
  parseJSON =
    Lude.withObject
      "Core"
      ( \x ->
          Core'
            Lude.<$> (x Lude..:? "SyncShadow")
            Lude.<*> (x Lude..: "ThingArn")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "CertificateArn")
      )

instance Lude.ToJSON Core where
  toJSON Core' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SyncShadow" Lude..=) Lude.<$> syncShadow,
            Lude.Just ("ThingArn" Lude..= thingARN),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("CertificateArn" Lude..= certificateARN)
          ]
      )
