{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.ChannelSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ChannelSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CompressionType
import Amazonka.SageMaker.Types.TrainingInputMode

-- | Defines a named input source, called a channel, to be used by an
-- algorithm.
--
-- /See:/ 'newChannelSpecification' smart constructor.
data ChannelSpecification = ChannelSpecification'
  { -- | A brief description of the channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the channel is required by the algorithm.
    isRequired :: Prelude.Maybe Prelude.Bool,
    -- | The allowed compression types, if data compression is used.
    supportedCompressionTypes :: Prelude.Maybe [CompressionType],
    -- | The name of the channel.
    name :: Prelude.Text,
    -- | The supported MIME types for the data.
    supportedContentTypes :: [Prelude.Text],
    -- | The allowed input mode, either FILE or PIPE.
    --
    -- In FILE mode, Amazon SageMaker copies the data from the input source
    -- onto the local Amazon Elastic Block Store (Amazon EBS) volumes before
    -- starting your training algorithm. This is the most commonly used input
    -- mode.
    --
    -- In PIPE mode, Amazon SageMaker streams input data from the source
    -- directly to your algorithm without using the EBS volume.
    supportedInputModes :: Prelude.NonEmpty TrainingInputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'channelSpecification_description' - A brief description of the channel.
--
-- 'isRequired', 'channelSpecification_isRequired' - Indicates whether the channel is required by the algorithm.
--
-- 'supportedCompressionTypes', 'channelSpecification_supportedCompressionTypes' - The allowed compression types, if data compression is used.
--
-- 'name', 'channelSpecification_name' - The name of the channel.
--
-- 'supportedContentTypes', 'channelSpecification_supportedContentTypes' - The supported MIME types for the data.
--
-- 'supportedInputModes', 'channelSpecification_supportedInputModes' - The allowed input mode, either FILE or PIPE.
--
-- In FILE mode, Amazon SageMaker copies the data from the input source
-- onto the local Amazon Elastic Block Store (Amazon EBS) volumes before
-- starting your training algorithm. This is the most commonly used input
-- mode.
--
-- In PIPE mode, Amazon SageMaker streams input data from the source
-- directly to your algorithm without using the EBS volume.
newChannelSpecification ::
  -- | 'name'
  Prelude.Text ->
  -- | 'supportedInputModes'
  Prelude.NonEmpty TrainingInputMode ->
  ChannelSpecification
newChannelSpecification pName_ pSupportedInputModes_ =
  ChannelSpecification'
    { description =
        Prelude.Nothing,
      isRequired = Prelude.Nothing,
      supportedCompressionTypes = Prelude.Nothing,
      name = pName_,
      supportedContentTypes = Prelude.mempty,
      supportedInputModes =
        Lens.coerced Lens.# pSupportedInputModes_
    }

-- | A brief description of the channel.
channelSpecification_description :: Lens.Lens' ChannelSpecification (Prelude.Maybe Prelude.Text)
channelSpecification_description = Lens.lens (\ChannelSpecification' {description} -> description) (\s@ChannelSpecification' {} a -> s {description = a} :: ChannelSpecification)

-- | Indicates whether the channel is required by the algorithm.
channelSpecification_isRequired :: Lens.Lens' ChannelSpecification (Prelude.Maybe Prelude.Bool)
channelSpecification_isRequired = Lens.lens (\ChannelSpecification' {isRequired} -> isRequired) (\s@ChannelSpecification' {} a -> s {isRequired = a} :: ChannelSpecification)

-- | The allowed compression types, if data compression is used.
channelSpecification_supportedCompressionTypes :: Lens.Lens' ChannelSpecification (Prelude.Maybe [CompressionType])
channelSpecification_supportedCompressionTypes = Lens.lens (\ChannelSpecification' {supportedCompressionTypes} -> supportedCompressionTypes) (\s@ChannelSpecification' {} a -> s {supportedCompressionTypes = a} :: ChannelSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel.
channelSpecification_name :: Lens.Lens' ChannelSpecification Prelude.Text
channelSpecification_name = Lens.lens (\ChannelSpecification' {name} -> name) (\s@ChannelSpecification' {} a -> s {name = a} :: ChannelSpecification)

-- | The supported MIME types for the data.
channelSpecification_supportedContentTypes :: Lens.Lens' ChannelSpecification [Prelude.Text]
channelSpecification_supportedContentTypes = Lens.lens (\ChannelSpecification' {supportedContentTypes} -> supportedContentTypes) (\s@ChannelSpecification' {} a -> s {supportedContentTypes = a} :: ChannelSpecification) Prelude.. Lens.coerced

-- | The allowed input mode, either FILE or PIPE.
--
-- In FILE mode, Amazon SageMaker copies the data from the input source
-- onto the local Amazon Elastic Block Store (Amazon EBS) volumes before
-- starting your training algorithm. This is the most commonly used input
-- mode.
--
-- In PIPE mode, Amazon SageMaker streams input data from the source
-- directly to your algorithm without using the EBS volume.
channelSpecification_supportedInputModes :: Lens.Lens' ChannelSpecification (Prelude.NonEmpty TrainingInputMode)
channelSpecification_supportedInputModes = Lens.lens (\ChannelSpecification' {supportedInputModes} -> supportedInputModes) (\s@ChannelSpecification' {} a -> s {supportedInputModes = a} :: ChannelSpecification) Prelude.. Lens.coerced

instance Data.FromJSON ChannelSpecification where
  parseJSON =
    Data.withObject
      "ChannelSpecification"
      ( \x ->
          ChannelSpecification'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsRequired")
            Prelude.<*> ( x Data..:? "SupportedCompressionTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> ( x Data..:? "SupportedContentTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "SupportedInputModes")
      )

instance Prelude.Hashable ChannelSpecification where
  hashWithSalt _salt ChannelSpecification' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isRequired
      `Prelude.hashWithSalt` supportedCompressionTypes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedContentTypes
      `Prelude.hashWithSalt` supportedInputModes

instance Prelude.NFData ChannelSpecification where
  rnf ChannelSpecification' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf isRequired
      `Prelude.seq` Prelude.rnf supportedCompressionTypes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedContentTypes
      `Prelude.seq` Prelude.rnf supportedInputModes

instance Data.ToJSON ChannelSpecification where
  toJSON ChannelSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("IsRequired" Data..=) Prelude.<$> isRequired,
            ("SupportedCompressionTypes" Data..=)
              Prelude.<$> supportedCompressionTypes,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "SupportedContentTypes"
                  Data..= supportedContentTypes
              ),
            Prelude.Just
              ("SupportedInputModes" Data..= supportedInputModes)
          ]
      )
