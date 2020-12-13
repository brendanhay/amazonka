{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLChannel
  ( AutoMLChannel (..),

    -- * Smart constructor
    mkAutoMLChannel,

    -- * Lenses
    amlcTargetAttributeName,
    amlcDataSource,
    amlcCompressionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLDataSource
import Network.AWS.SageMaker.Types.CompressionType

-- | Similar to Channel. A channel is a named input source that training algorithms can consume. Refer to Channel for detailed descriptions.
--
-- /See:/ 'mkAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { -- | The name of the target variable in supervised learning, a.k.a. 'y'.
    targetAttributeName :: Lude.Text,
    -- | The data source.
    dataSource :: AutoMLDataSource,
    -- | You can use Gzip or None. The default value is None.
    compressionType :: Lude.Maybe CompressionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLChannel' with the minimum fields required to make a request.
--
-- * 'targetAttributeName' - The name of the target variable in supervised learning, a.k.a. 'y'.
-- * 'dataSource' - The data source.
-- * 'compressionType' - You can use Gzip or None. The default value is None.
mkAutoMLChannel ::
  -- | 'targetAttributeName'
  Lude.Text ->
  -- | 'dataSource'
  AutoMLDataSource ->
  AutoMLChannel
mkAutoMLChannel pTargetAttributeName_ pDataSource_ =
  AutoMLChannel'
    { targetAttributeName = pTargetAttributeName_,
      dataSource = pDataSource_,
      compressionType = Lude.Nothing
    }

-- | The name of the target variable in supervised learning, a.k.a. 'y'.
--
-- /Note:/ Consider using 'targetAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcTargetAttributeName :: Lens.Lens' AutoMLChannel Lude.Text
amlcTargetAttributeName = Lens.lens (targetAttributeName :: AutoMLChannel -> Lude.Text) (\s a -> s {targetAttributeName = a} :: AutoMLChannel)
{-# DEPRECATED amlcTargetAttributeName "Use generic-lens or generic-optics with 'targetAttributeName' instead." #-}

-- | The data source.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcDataSource :: Lens.Lens' AutoMLChannel AutoMLDataSource
amlcDataSource = Lens.lens (dataSource :: AutoMLChannel -> AutoMLDataSource) (\s a -> s {dataSource = a} :: AutoMLChannel)
{-# DEPRECATED amlcDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | You can use Gzip or None. The default value is None.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCompressionType :: Lens.Lens' AutoMLChannel (Lude.Maybe CompressionType)
amlcCompressionType = Lens.lens (compressionType :: AutoMLChannel -> Lude.Maybe CompressionType) (\s a -> s {compressionType = a} :: AutoMLChannel)
{-# DEPRECATED amlcCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

instance Lude.FromJSON AutoMLChannel where
  parseJSON =
    Lude.withObject
      "AutoMLChannel"
      ( \x ->
          AutoMLChannel'
            Lude.<$> (x Lude..: "TargetAttributeName")
            Lude.<*> (x Lude..: "DataSource")
            Lude.<*> (x Lude..:? "CompressionType")
      )

instance Lude.ToJSON AutoMLChannel where
  toJSON AutoMLChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetAttributeName" Lude..= targetAttributeName),
            Lude.Just ("DataSource" Lude..= dataSource),
            ("CompressionType" Lude..=) Lude.<$> compressionType
          ]
      )
