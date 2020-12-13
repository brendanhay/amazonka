{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SigV4Authorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SigV4Authorization
  ( SigV4Authorization (..),

    -- * Smart constructor
    mkSigV4Authorization,

    -- * Lenses
    svaServiceName,
    svaSigningRegion,
    svaRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 signing process> .
--
-- /See:/ 'mkSigV4Authorization' smart constructor.
data SigV4Authorization = SigV4Authorization'
  { -- | The service name to use while signing with Sig V4.
    serviceName :: Lude.Text,
    -- | The signing region.
    signingRegion :: Lude.Text,
    -- | The ARN of the signing role.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SigV4Authorization' with the minimum fields required to make a request.
--
-- * 'serviceName' - The service name to use while signing with Sig V4.
-- * 'signingRegion' - The signing region.
-- * 'roleARN' - The ARN of the signing role.
mkSigV4Authorization ::
  -- | 'serviceName'
  Lude.Text ->
  -- | 'signingRegion'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  SigV4Authorization
mkSigV4Authorization pServiceName_ pSigningRegion_ pRoleARN_ =
  SigV4Authorization'
    { serviceName = pServiceName_,
      signingRegion = pSigningRegion_,
      roleARN = pRoleARN_
    }

-- | The service name to use while signing with Sig V4.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svaServiceName :: Lens.Lens' SigV4Authorization Lude.Text
svaServiceName = Lens.lens (serviceName :: SigV4Authorization -> Lude.Text) (\s a -> s {serviceName = a} :: SigV4Authorization)
{-# DEPRECATED svaServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The signing region.
--
-- /Note:/ Consider using 'signingRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svaSigningRegion :: Lens.Lens' SigV4Authorization Lude.Text
svaSigningRegion = Lens.lens (signingRegion :: SigV4Authorization -> Lude.Text) (\s a -> s {signingRegion = a} :: SigV4Authorization)
{-# DEPRECATED svaSigningRegion "Use generic-lens or generic-optics with 'signingRegion' instead." #-}

-- | The ARN of the signing role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svaRoleARN :: Lens.Lens' SigV4Authorization Lude.Text
svaRoleARN = Lens.lens (roleARN :: SigV4Authorization -> Lude.Text) (\s a -> s {roleARN = a} :: SigV4Authorization)
{-# DEPRECATED svaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SigV4Authorization where
  parseJSON =
    Lude.withObject
      "SigV4Authorization"
      ( \x ->
          SigV4Authorization'
            Lude.<$> (x Lude..: "serviceName")
            Lude.<*> (x Lude..: "signingRegion")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON SigV4Authorization where
  toJSON SigV4Authorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("serviceName" Lude..= serviceName),
            Lude.Just ("signingRegion" Lude..= signingRegion),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
