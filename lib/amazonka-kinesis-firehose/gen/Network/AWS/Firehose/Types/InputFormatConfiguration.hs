{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.InputFormatConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.InputFormatConfiguration
  ( InputFormatConfiguration (..),

    -- * Smart constructor
    mkInputFormatConfiguration,

    -- * Lenses
    ifcDeserializer,
  )
where

import Network.AWS.Firehose.Types.Deserializer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the deserializer you want to use to convert the format of the input data. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'mkInputFormatConfiguration' smart constructor.
newtype InputFormatConfiguration = InputFormatConfiguration'
  { -- | Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
    deserializer :: Lude.Maybe Deserializer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputFormatConfiguration' with the minimum fields required to make a request.
--
-- * 'deserializer' - Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
mkInputFormatConfiguration ::
  InputFormatConfiguration
mkInputFormatConfiguration =
  InputFormatConfiguration' {deserializer = Lude.Nothing}

-- | Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
--
-- /Note:/ Consider using 'deserializer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcDeserializer :: Lens.Lens' InputFormatConfiguration (Lude.Maybe Deserializer)
ifcDeserializer = Lens.lens (deserializer :: InputFormatConfiguration -> Lude.Maybe Deserializer) (\s a -> s {deserializer = a} :: InputFormatConfiguration)
{-# DEPRECATED ifcDeserializer "Use generic-lens or generic-optics with 'deserializer' instead." #-}

instance Lude.FromJSON InputFormatConfiguration where
  parseJSON =
    Lude.withObject
      "InputFormatConfiguration"
      ( \x ->
          InputFormatConfiguration' Lude.<$> (x Lude..:? "Deserializer")
      )

instance Lude.ToJSON InputFormatConfiguration where
  toJSON InputFormatConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("Deserializer" Lude..=) Lude.<$> deserializer])
