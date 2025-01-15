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
-- Module      : Amazonka.Braket.Types.InputFileConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.InputFileConfig where

import Amazonka.Braket.Types.DataSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of parameters that specify the input channels, type of input
-- data, and where it is located.
--
-- /See:/ 'newInputFileConfig' smart constructor.
data InputFileConfig = InputFileConfig'
  { -- | The MIME type of the data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | A named input source that an Amazon Braket job can consume.
    channelName :: Prelude.Text,
    -- | The location of the channel data.
    dataSource :: DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputFileConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'inputFileConfig_contentType' - The MIME type of the data.
--
-- 'channelName', 'inputFileConfig_channelName' - A named input source that an Amazon Braket job can consume.
--
-- 'dataSource', 'inputFileConfig_dataSource' - The location of the channel data.
newInputFileConfig ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'dataSource'
  DataSource ->
  InputFileConfig
newInputFileConfig pChannelName_ pDataSource_ =
  InputFileConfig'
    { contentType = Prelude.Nothing,
      channelName = pChannelName_,
      dataSource = pDataSource_
    }

-- | The MIME type of the data.
inputFileConfig_contentType :: Lens.Lens' InputFileConfig (Prelude.Maybe Prelude.Text)
inputFileConfig_contentType = Lens.lens (\InputFileConfig' {contentType} -> contentType) (\s@InputFileConfig' {} a -> s {contentType = a} :: InputFileConfig)

-- | A named input source that an Amazon Braket job can consume.
inputFileConfig_channelName :: Lens.Lens' InputFileConfig Prelude.Text
inputFileConfig_channelName = Lens.lens (\InputFileConfig' {channelName} -> channelName) (\s@InputFileConfig' {} a -> s {channelName = a} :: InputFileConfig)

-- | The location of the channel data.
inputFileConfig_dataSource :: Lens.Lens' InputFileConfig DataSource
inputFileConfig_dataSource = Lens.lens (\InputFileConfig' {dataSource} -> dataSource) (\s@InputFileConfig' {} a -> s {dataSource = a} :: InputFileConfig)

instance Data.FromJSON InputFileConfig where
  parseJSON =
    Data.withObject
      "InputFileConfig"
      ( \x ->
          InputFileConfig'
            Prelude.<$> (x Data..:? "contentType")
            Prelude.<*> (x Data..: "channelName")
            Prelude.<*> (x Data..: "dataSource")
      )

instance Prelude.Hashable InputFileConfig where
  hashWithSalt _salt InputFileConfig' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData InputFileConfig where
  rnf InputFileConfig' {..} =
    Prelude.rnf contentType `Prelude.seq`
      Prelude.rnf channelName `Prelude.seq`
        Prelude.rnf dataSource

instance Data.ToJSON InputFileConfig where
  toJSON InputFileConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentType" Data..=) Prelude.<$> contentType,
            Prelude.Just ("channelName" Data..= channelName),
            Prelude.Just ("dataSource" Data..= dataSource)
          ]
      )
