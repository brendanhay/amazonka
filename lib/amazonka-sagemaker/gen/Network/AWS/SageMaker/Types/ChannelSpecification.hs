{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ChannelSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ChannelSpecification
  ( ChannelSpecification (..),

    -- * Smart constructor
    mkChannelSpecification,

    -- * Lenses
    csSupportedContentTypes,
    csSupportedInputModes,
    csSupportedCompressionTypes,
    csName,
    csIsRequired,
    csDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Defines a named input source, called a channel, to be used by an algorithm.
--
-- /See:/ 'mkChannelSpecification' smart constructor.
data ChannelSpecification = ChannelSpecification'
  { -- | The supported MIME types for the data.
    supportedContentTypes :: [Lude.Text],
    -- | The allowed input mode, either FILE or PIPE.
    --
    -- In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode.
    -- In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
    supportedInputModes :: Lude.NonEmpty TrainingInputMode,
    -- | The allowed compression types, if data compression is used.
    supportedCompressionTypes :: Lude.Maybe [CompressionType],
    -- | The name of the channel.
    name :: Lude.Text,
    -- | Indicates whether the channel is required by the algorithm.
    isRequired :: Lude.Maybe Lude.Bool,
    -- | A brief description of the channel.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelSpecification' with the minimum fields required to make a request.
--
-- * 'supportedContentTypes' - The supported MIME types for the data.
-- * 'supportedInputModes' - The allowed input mode, either FILE or PIPE.
--
-- In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode.
-- In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
-- * 'supportedCompressionTypes' - The allowed compression types, if data compression is used.
-- * 'name' - The name of the channel.
-- * 'isRequired' - Indicates whether the channel is required by the algorithm.
-- * 'description' - A brief description of the channel.
mkChannelSpecification ::
  -- | 'supportedInputModes'
  Lude.NonEmpty TrainingInputMode ->
  -- | 'name'
  Lude.Text ->
  ChannelSpecification
mkChannelSpecification pSupportedInputModes_ pName_ =
  ChannelSpecification'
    { supportedContentTypes = Lude.mempty,
      supportedInputModes = pSupportedInputModes_,
      supportedCompressionTypes = Lude.Nothing,
      name = pName_,
      isRequired = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The supported MIME types for the data.
--
-- /Note:/ Consider using 'supportedContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSupportedContentTypes :: Lens.Lens' ChannelSpecification [Lude.Text]
csSupportedContentTypes = Lens.lens (supportedContentTypes :: ChannelSpecification -> [Lude.Text]) (\s a -> s {supportedContentTypes = a} :: ChannelSpecification)
{-# DEPRECATED csSupportedContentTypes "Use generic-lens or generic-optics with 'supportedContentTypes' instead." #-}

-- | The allowed input mode, either FILE or PIPE.
--
-- In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode.
-- In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
--
-- /Note:/ Consider using 'supportedInputModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSupportedInputModes :: Lens.Lens' ChannelSpecification (Lude.NonEmpty TrainingInputMode)
csSupportedInputModes = Lens.lens (supportedInputModes :: ChannelSpecification -> Lude.NonEmpty TrainingInputMode) (\s a -> s {supportedInputModes = a} :: ChannelSpecification)
{-# DEPRECATED csSupportedInputModes "Use generic-lens or generic-optics with 'supportedInputModes' instead." #-}

-- | The allowed compression types, if data compression is used.
--
-- /Note:/ Consider using 'supportedCompressionTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSupportedCompressionTypes :: Lens.Lens' ChannelSpecification (Lude.Maybe [CompressionType])
csSupportedCompressionTypes = Lens.lens (supportedCompressionTypes :: ChannelSpecification -> Lude.Maybe [CompressionType]) (\s a -> s {supportedCompressionTypes = a} :: ChannelSpecification)
{-# DEPRECATED csSupportedCompressionTypes "Use generic-lens or generic-optics with 'supportedCompressionTypes' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ChannelSpecification Lude.Text
csName = Lens.lens (name :: ChannelSpecification -> Lude.Text) (\s a -> s {name = a} :: ChannelSpecification)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Indicates whether the channel is required by the algorithm.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIsRequired :: Lens.Lens' ChannelSpecification (Lude.Maybe Lude.Bool)
csIsRequired = Lens.lens (isRequired :: ChannelSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {isRequired = a} :: ChannelSpecification)
{-# DEPRECATED csIsRequired "Use generic-lens or generic-optics with 'isRequired' instead." #-}

-- | A brief description of the channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' ChannelSpecification (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: ChannelSpecification -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ChannelSpecification)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ChannelSpecification where
  parseJSON =
    Lude.withObject
      "ChannelSpecification"
      ( \x ->
          ChannelSpecification'
            Lude.<$> (x Lude..:? "SupportedContentTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "SupportedInputModes")
            Lude.<*> (x Lude..:? "SupportedCompressionTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "IsRequired")
            Lude.<*> (x Lude..:? "Description")
      )

instance Lude.ToJSON ChannelSpecification where
  toJSON ChannelSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SupportedContentTypes" Lude..= supportedContentTypes),
            Lude.Just ("SupportedInputModes" Lude..= supportedInputModes),
            ("SupportedCompressionTypes" Lude..=)
              Lude.<$> supportedCompressionTypes,
            Lude.Just ("Name" Lude..= name),
            ("IsRequired" Lude..=) Lude.<$> isRequired,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
