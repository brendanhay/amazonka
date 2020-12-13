{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerEngineType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerEngineType
  ( BrokerEngineType (..),

    -- * Smart constructor
    mkBrokerEngineType,

    -- * Lenses
    betEngineVersions,
    betEngineType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.EngineType
import Network.AWS.MQ.Types.EngineVersion
import qualified Network.AWS.Prelude as Lude

-- | Types of broker engines.
--
-- /See:/ 'mkBrokerEngineType' smart constructor.
data BrokerEngineType = BrokerEngineType'
  { -- | The list of engine versions.
    engineVersions :: Lude.Maybe [EngineVersion],
    -- | The type of broker engine.
    engineType :: Lude.Maybe EngineType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BrokerEngineType' with the minimum fields required to make a request.
--
-- * 'engineVersions' - The list of engine versions.
-- * 'engineType' - The type of broker engine.
mkBrokerEngineType ::
  BrokerEngineType
mkBrokerEngineType =
  BrokerEngineType'
    { engineVersions = Lude.Nothing,
      engineType = Lude.Nothing
    }

-- | The list of engine versions.
--
-- /Note:/ Consider using 'engineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
betEngineVersions :: Lens.Lens' BrokerEngineType (Lude.Maybe [EngineVersion])
betEngineVersions = Lens.lens (engineVersions :: BrokerEngineType -> Lude.Maybe [EngineVersion]) (\s a -> s {engineVersions = a} :: BrokerEngineType)
{-# DEPRECATED betEngineVersions "Use generic-lens or generic-optics with 'engineVersions' instead." #-}

-- | The type of broker engine.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
betEngineType :: Lens.Lens' BrokerEngineType (Lude.Maybe EngineType)
betEngineType = Lens.lens (engineType :: BrokerEngineType -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: BrokerEngineType)
{-# DEPRECATED betEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

instance Lude.FromJSON BrokerEngineType where
  parseJSON =
    Lude.withObject
      "BrokerEngineType"
      ( \x ->
          BrokerEngineType'
            Lude.<$> (x Lude..:? "engineVersions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "engineType")
      )
