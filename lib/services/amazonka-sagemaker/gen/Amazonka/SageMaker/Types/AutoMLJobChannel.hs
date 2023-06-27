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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLChannelType
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.CompressionType

-- | A channel is a named input source that training algorithms can consume.
-- This channel is used for AutoML jobs V2 (jobs created by calling
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJobV2.html CreateAutoMLJobV2>).
--
-- /See:/ 'newAutoMLJobChannel' smart constructor.
data AutoMLJobChannel = AutoMLJobChannel'
  { -- | The type of channel. Defines whether the data are used for training or
    -- validation. The default value is @training@. Channels for @training@ and
    -- @validation@ must share the same @ContentType@
    channelType :: Prelude.Maybe AutoMLChannelType,
    -- | The allowed compression types depend on the input format and problem
    -- type. We allow the compression type @Gzip@ for @S3Prefix@ inputs on
    -- tabular data only. For all other inputs, the compression type should be
    -- @None@. If no compression type is provided, we default to @None@.
    compressionType :: Prelude.Maybe CompressionType,
    -- | The content type of the data from the input source. The following are
    -- the allowed content types for different problems:
    --
    -- -   For Tabular problem types: @text\/csv;header=present@ or
    --     @x-application\/vnd.amazon+parquet@. The default value is
    --     @text\/csv;header=present@.
    --
    -- -   For ImageClassification: @image\/png@, @image\/jpeg@, or @image\/*@.
    --     The default value is @image\/*@.
    --
    -- -   For TextClassification: @text\/csv;header=present@ or
    --     @x-application\/vnd.amazon+parquet@. The default value is
    --     @text\/csv;header=present@.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The data source for an AutoML channel (Required).
    dataSource :: Prelude.Maybe AutoMLDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelType', 'autoMLJobChannel_channelType' - The type of channel. Defines whether the data are used for training or
-- validation. The default value is @training@. Channels for @training@ and
-- @validation@ must share the same @ContentType@
--
-- 'compressionType', 'autoMLJobChannel_compressionType' - The allowed compression types depend on the input format and problem
-- type. We allow the compression type @Gzip@ for @S3Prefix@ inputs on
-- tabular data only. For all other inputs, the compression type should be
-- @None@. If no compression type is provided, we default to @None@.
--
-- 'contentType', 'autoMLJobChannel_contentType' - The content type of the data from the input source. The following are
-- the allowed content types for different problems:
--
-- -   For Tabular problem types: @text\/csv;header=present@ or
--     @x-application\/vnd.amazon+parquet@. The default value is
--     @text\/csv;header=present@.
--
-- -   For ImageClassification: @image\/png@, @image\/jpeg@, or @image\/*@.
--     The default value is @image\/*@.
--
-- -   For TextClassification: @text\/csv;header=present@ or
--     @x-application\/vnd.amazon+parquet@. The default value is
--     @text\/csv;header=present@.
--
-- 'dataSource', 'autoMLJobChannel_dataSource' - The data source for an AutoML channel (Required).
newAutoMLJobChannel ::
  AutoMLJobChannel
newAutoMLJobChannel =
  AutoMLJobChannel'
    { channelType = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      contentType = Prelude.Nothing,
      dataSource = Prelude.Nothing
    }

-- | The type of channel. Defines whether the data are used for training or
-- validation. The default value is @training@. Channels for @training@ and
-- @validation@ must share the same @ContentType@
autoMLJobChannel_channelType :: Lens.Lens' AutoMLJobChannel (Prelude.Maybe AutoMLChannelType)
autoMLJobChannel_channelType = Lens.lens (\AutoMLJobChannel' {channelType} -> channelType) (\s@AutoMLJobChannel' {} a -> s {channelType = a} :: AutoMLJobChannel)

-- | The allowed compression types depend on the input format and problem
-- type. We allow the compression type @Gzip@ for @S3Prefix@ inputs on
-- tabular data only. For all other inputs, the compression type should be
-- @None@. If no compression type is provided, we default to @None@.
autoMLJobChannel_compressionType :: Lens.Lens' AutoMLJobChannel (Prelude.Maybe CompressionType)
autoMLJobChannel_compressionType = Lens.lens (\AutoMLJobChannel' {compressionType} -> compressionType) (\s@AutoMLJobChannel' {} a -> s {compressionType = a} :: AutoMLJobChannel)

-- | The content type of the data from the input source. The following are
-- the allowed content types for different problems:
--
-- -   For Tabular problem types: @text\/csv;header=present@ or
--     @x-application\/vnd.amazon+parquet@. The default value is
--     @text\/csv;header=present@.
--
-- -   For ImageClassification: @image\/png@, @image\/jpeg@, or @image\/*@.
--     The default value is @image\/*@.
--
-- -   For TextClassification: @text\/csv;header=present@ or
--     @x-application\/vnd.amazon+parquet@. The default value is
--     @text\/csv;header=present@.
autoMLJobChannel_contentType :: Lens.Lens' AutoMLJobChannel (Prelude.Maybe Prelude.Text)
autoMLJobChannel_contentType = Lens.lens (\AutoMLJobChannel' {contentType} -> contentType) (\s@AutoMLJobChannel' {} a -> s {contentType = a} :: AutoMLJobChannel)

-- | The data source for an AutoML channel (Required).
autoMLJobChannel_dataSource :: Lens.Lens' AutoMLJobChannel (Prelude.Maybe AutoMLDataSource)
autoMLJobChannel_dataSource = Lens.lens (\AutoMLJobChannel' {dataSource} -> dataSource) (\s@AutoMLJobChannel' {} a -> s {dataSource = a} :: AutoMLJobChannel)

instance Data.FromJSON AutoMLJobChannel where
  parseJSON =
    Data.withObject
      "AutoMLJobChannel"
      ( \x ->
          AutoMLJobChannel'
            Prelude.<$> (x Data..:? "ChannelType")
            Prelude.<*> (x Data..:? "CompressionType")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "DataSource")
      )

instance Prelude.Hashable AutoMLJobChannel where
  hashWithSalt _salt AutoMLJobChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData AutoMLJobChannel where
  rnf AutoMLJobChannel' {..} =
    Prelude.rnf channelType
      `Prelude.seq` Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf dataSource

instance Data.ToJSON AutoMLJobChannel where
  toJSON AutoMLJobChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelType" Data..=) Prelude.<$> channelType,
            ("CompressionType" Data..=)
              Prelude.<$> compressionType,
            ("ContentType" Data..=) Prelude.<$> contentType,
            ("DataSource" Data..=) Prelude.<$> dataSource
          ]
      )
