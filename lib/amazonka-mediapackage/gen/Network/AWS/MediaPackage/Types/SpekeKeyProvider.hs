{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SpekeKeyProvider
  ( SpekeKeyProvider (..),

    -- * Smart constructor
    mkSpekeKeyProvider,

    -- * Lenses
    skpCertificateARN,
    skpResourceId,
    skpSystemIds,
    skpURL,
    skpRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'mkSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    resourceId :: Lude.Text,
    systemIds :: [Lude.Text],
    url :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- * 'certificateARN' - An Amazon Resource Name (ARN) of a Certificate Manager certificate
--
-- that MediaPackage will use for enforcing secure end-to-end data
-- transfer with the key provider service.
-- * 'resourceId' - The resource ID to include in key requests.
-- * 'roleARN' - An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
--
-- MediaPackage will assume when accessing the key provider service.
-- * 'systemIds' - The system IDs to include in key requests.
-- * 'url' - The URL of the external key provider service.
mkSpekeKeyProvider ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'url'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  SpekeKeyProvider
mkSpekeKeyProvider pResourceId_ pURL_ pRoleARN_ =
  SpekeKeyProvider'
    { certificateARN = Lude.Nothing,
      resourceId = pResourceId_,
      systemIds = Lude.mempty,
      url = pURL_,
      roleARN = pRoleARN_
    }

-- | An Amazon Resource Name (ARN) of a Certificate Manager certificate
--
-- that MediaPackage will use for enforcing secure end-to-end data
-- transfer with the key provider service.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpCertificateARN :: Lens.Lens' SpekeKeyProvider (Lude.Maybe Lude.Text)
skpCertificateARN = Lens.lens (certificateARN :: SpekeKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: SpekeKeyProvider)
{-# DEPRECATED skpCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The resource ID to include in key requests.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpResourceId :: Lens.Lens' SpekeKeyProvider Lude.Text
skpResourceId = Lens.lens (resourceId :: SpekeKeyProvider -> Lude.Text) (\s a -> s {resourceId = a} :: SpekeKeyProvider)
{-# DEPRECATED skpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The system IDs to include in key requests.
--
-- /Note:/ Consider using 'systemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpSystemIds :: Lens.Lens' SpekeKeyProvider [Lude.Text]
skpSystemIds = Lens.lens (systemIds :: SpekeKeyProvider -> [Lude.Text]) (\s a -> s {systemIds = a} :: SpekeKeyProvider)
{-# DEPRECATED skpSystemIds "Use generic-lens or generic-optics with 'systemIds' instead." #-}

-- | The URL of the external key provider service.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpURL :: Lens.Lens' SpekeKeyProvider Lude.Text
skpURL = Lens.lens (url :: SpekeKeyProvider -> Lude.Text) (\s a -> s {url = a} :: SpekeKeyProvider)
{-# DEPRECATED skpURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
--
-- MediaPackage will assume when accessing the key provider service.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpRoleARN :: Lens.Lens' SpekeKeyProvider Lude.Text
skpRoleARN = Lens.lens (roleARN :: SpekeKeyProvider -> Lude.Text) (\s a -> s {roleARN = a} :: SpekeKeyProvider)
{-# DEPRECATED skpRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SpekeKeyProvider where
  parseJSON =
    Lude.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Lude.<$> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..: "resourceId")
            Lude.<*> (x Lude..:? "systemIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "url")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("certificateArn" Lude..=) Lude.<$> certificateARN,
            Lude.Just ("resourceId" Lude..= resourceId),
            Lude.Just ("systemIds" Lude..= systemIds),
            Lude.Just ("url" Lude..= url),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
