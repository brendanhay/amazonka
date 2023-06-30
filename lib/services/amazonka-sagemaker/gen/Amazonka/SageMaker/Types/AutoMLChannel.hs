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
-- Module      : Amazonka.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLChannelType
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.CompressionType

-- | A channel is a named input source that training algorithms can consume.
-- The validation dataset size is limited to less than 2 GB. The training
-- dataset size must be less than 100 GB. For more information, see .
--
-- A validation dataset must contain the same headers as the training
-- dataset.
--
-- /See:/ 'newAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { -- | The channel type (optional) is an @enum@ string. The default value is
    -- @training@. Channels for training and validation must share the same
    -- @ContentType@ and @TargetAttributeName@. For information on specifying
    -- training and validation channel types, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-data-sources-training-or-validation How to specify training and validation datasets>
    -- .
    channelType :: Prelude.Maybe AutoMLChannelType,
    -- | You can use @Gzip@ or @None@. The default value is @None@.
    compressionType :: Prelude.Maybe CompressionType,
    -- | The content type of the data from the input source. You can use
    -- @text\/csv;header=present@ or @x-application\/vnd.amazon+parquet@. The
    -- default value is @text\/csv;header=present@.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The data source for an AutoML channel.
    dataSource :: AutoMLDataSource,
    -- | The name of the target variable in supervised learning, usually
    -- represented by \'y\'.
    targetAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelType', 'autoMLChannel_channelType' - The channel type (optional) is an @enum@ string. The default value is
-- @training@. Channels for training and validation must share the same
-- @ContentType@ and @TargetAttributeName@. For information on specifying
-- training and validation channel types, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-data-sources-training-or-validation How to specify training and validation datasets>
-- .
--
-- 'compressionType', 'autoMLChannel_compressionType' - You can use @Gzip@ or @None@. The default value is @None@.
--
-- 'contentType', 'autoMLChannel_contentType' - The content type of the data from the input source. You can use
-- @text\/csv;header=present@ or @x-application\/vnd.amazon+parquet@. The
-- default value is @text\/csv;header=present@.
--
-- 'dataSource', 'autoMLChannel_dataSource' - The data source for an AutoML channel.
--
-- 'targetAttributeName', 'autoMLChannel_targetAttributeName' - The name of the target variable in supervised learning, usually
-- represented by \'y\'.
newAutoMLChannel ::
  -- | 'dataSource'
  AutoMLDataSource ->
  -- | 'targetAttributeName'
  Prelude.Text ->
  AutoMLChannel
newAutoMLChannel pDataSource_ pTargetAttributeName_ =
  AutoMLChannel'
    { channelType = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      contentType = Prelude.Nothing,
      dataSource = pDataSource_,
      targetAttributeName = pTargetAttributeName_
    }

-- | The channel type (optional) is an @enum@ string. The default value is
-- @training@. Channels for training and validation must share the same
-- @ContentType@ and @TargetAttributeName@. For information on specifying
-- training and validation channel types, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-data-sources-training-or-validation How to specify training and validation datasets>
-- .
autoMLChannel_channelType :: Lens.Lens' AutoMLChannel (Prelude.Maybe AutoMLChannelType)
autoMLChannel_channelType = Lens.lens (\AutoMLChannel' {channelType} -> channelType) (\s@AutoMLChannel' {} a -> s {channelType = a} :: AutoMLChannel)

-- | You can use @Gzip@ or @None@. The default value is @None@.
autoMLChannel_compressionType :: Lens.Lens' AutoMLChannel (Prelude.Maybe CompressionType)
autoMLChannel_compressionType = Lens.lens (\AutoMLChannel' {compressionType} -> compressionType) (\s@AutoMLChannel' {} a -> s {compressionType = a} :: AutoMLChannel)

-- | The content type of the data from the input source. You can use
-- @text\/csv;header=present@ or @x-application\/vnd.amazon+parquet@. The
-- default value is @text\/csv;header=present@.
autoMLChannel_contentType :: Lens.Lens' AutoMLChannel (Prelude.Maybe Prelude.Text)
autoMLChannel_contentType = Lens.lens (\AutoMLChannel' {contentType} -> contentType) (\s@AutoMLChannel' {} a -> s {contentType = a} :: AutoMLChannel)

-- | The data source for an AutoML channel.
autoMLChannel_dataSource :: Lens.Lens' AutoMLChannel AutoMLDataSource
autoMLChannel_dataSource = Lens.lens (\AutoMLChannel' {dataSource} -> dataSource) (\s@AutoMLChannel' {} a -> s {dataSource = a} :: AutoMLChannel)

-- | The name of the target variable in supervised learning, usually
-- represented by \'y\'.
autoMLChannel_targetAttributeName :: Lens.Lens' AutoMLChannel Prelude.Text
autoMLChannel_targetAttributeName = Lens.lens (\AutoMLChannel' {targetAttributeName} -> targetAttributeName) (\s@AutoMLChannel' {} a -> s {targetAttributeName = a} :: AutoMLChannel)

instance Data.FromJSON AutoMLChannel where
  parseJSON =
    Data.withObject
      "AutoMLChannel"
      ( \x ->
          AutoMLChannel'
            Prelude.<$> (x Data..:? "ChannelType")
            Prelude.<*> (x Data..:? "CompressionType")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "TargetAttributeName")
      )

instance Prelude.Hashable AutoMLChannel where
  hashWithSalt _salt AutoMLChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` targetAttributeName

instance Prelude.NFData AutoMLChannel where
  rnf AutoMLChannel' {..} =
    Prelude.rnf channelType
      `Prelude.seq` Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf targetAttributeName

instance Data.ToJSON AutoMLChannel where
  toJSON AutoMLChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelType" Data..=) Prelude.<$> channelType,
            ("CompressionType" Data..=)
              Prelude.<$> compressionType,
            ("ContentType" Data..=) Prelude.<$> contentType,
            Prelude.Just ("DataSource" Data..= dataSource),
            Prelude.Just
              ("TargetAttributeName" Data..= targetAttributeName)
          ]
      )
