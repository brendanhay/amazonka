-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
  ( MultiplexProgramServiceDescriptor (..),

    -- * Smart constructor
    mkMultiplexProgramServiceDescriptor,

    -- * Lenses
    mpsdProviderName,
    mpsdServiceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Transport stream service descriptor configuration for the Multiplex program.
--
-- /See:/ 'mkMultiplexProgramServiceDescriptor' smart constructor.
data MultiplexProgramServiceDescriptor = MultiplexProgramServiceDescriptor'
  { providerName ::
      Lude.Text,
    serviceName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgramServiceDescriptor' with the minimum fields required to make a request.
--
-- * 'providerName' - Name of the provider.
-- * 'serviceName' - Name of the service.
mkMultiplexProgramServiceDescriptor ::
  -- | 'providerName'
  Lude.Text ->
  -- | 'serviceName'
  Lude.Text ->
  MultiplexProgramServiceDescriptor
mkMultiplexProgramServiceDescriptor pProviderName_ pServiceName_ =
  MultiplexProgramServiceDescriptor'
    { providerName = pProviderName_,
      serviceName = pServiceName_
    }

-- | Name of the provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdProviderName :: Lens.Lens' MultiplexProgramServiceDescriptor Lude.Text
mpsdProviderName = Lens.lens (providerName :: MultiplexProgramServiceDescriptor -> Lude.Text) (\s a -> s {providerName = a} :: MultiplexProgramServiceDescriptor)
{-# DEPRECATED mpsdProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | Name of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdServiceName :: Lens.Lens' MultiplexProgramServiceDescriptor Lude.Text
mpsdServiceName = Lens.lens (serviceName :: MultiplexProgramServiceDescriptor -> Lude.Text) (\s a -> s {serviceName = a} :: MultiplexProgramServiceDescriptor)
{-# DEPRECATED mpsdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.FromJSON MultiplexProgramServiceDescriptor where
  parseJSON =
    Lude.withObject
      "MultiplexProgramServiceDescriptor"
      ( \x ->
          MultiplexProgramServiceDescriptor'
            Lude.<$> (x Lude..: "providerName") Lude.<*> (x Lude..: "serviceName")
      )

instance Lude.ToJSON MultiplexProgramServiceDescriptor where
  toJSON MultiplexProgramServiceDescriptor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("providerName" Lude..= providerName),
            Lude.Just ("serviceName" Lude..= serviceName)
          ]
      )
