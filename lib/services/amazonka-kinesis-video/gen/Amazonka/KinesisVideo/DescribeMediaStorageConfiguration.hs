{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideo.DescribeMediaStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the channel. Specify the
-- @ChannelName@ or @ChannelARN@ in the input.
module Amazonka.KinesisVideo.DescribeMediaStorageConfiguration
  ( -- * Creating a Request
    DescribeMediaStorageConfiguration (..),
    newDescribeMediaStorageConfiguration,

    -- * Request Lenses
    describeMediaStorageConfiguration_channelARN,
    describeMediaStorageConfiguration_channelName,

    -- * Destructuring the Response
    DescribeMediaStorageConfigurationResponse (..),
    newDescribeMediaStorageConfigurationResponse,

    -- * Response Lenses
    describeMediaStorageConfigurationResponse_mediaStorageConfiguration,
    describeMediaStorageConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMediaStorageConfiguration' smart constructor.
data DescribeMediaStorageConfiguration = DescribeMediaStorageConfiguration'
  { -- | The Amazon Resource Name (ARN) of the channel.
    channelARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMediaStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelARN', 'describeMediaStorageConfiguration_channelARN' - The Amazon Resource Name (ARN) of the channel.
--
-- 'channelName', 'describeMediaStorageConfiguration_channelName' - The name of the channel.
newDescribeMediaStorageConfiguration ::
  DescribeMediaStorageConfiguration
newDescribeMediaStorageConfiguration =
  DescribeMediaStorageConfiguration'
    { channelARN =
        Prelude.Nothing,
      channelName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the channel.
describeMediaStorageConfiguration_channelARN :: Lens.Lens' DescribeMediaStorageConfiguration (Prelude.Maybe Prelude.Text)
describeMediaStorageConfiguration_channelARN = Lens.lens (\DescribeMediaStorageConfiguration' {channelARN} -> channelARN) (\s@DescribeMediaStorageConfiguration' {} a -> s {channelARN = a} :: DescribeMediaStorageConfiguration)

-- | The name of the channel.
describeMediaStorageConfiguration_channelName :: Lens.Lens' DescribeMediaStorageConfiguration (Prelude.Maybe Prelude.Text)
describeMediaStorageConfiguration_channelName = Lens.lens (\DescribeMediaStorageConfiguration' {channelName} -> channelName) (\s@DescribeMediaStorageConfiguration' {} a -> s {channelName = a} :: DescribeMediaStorageConfiguration)

instance
  Core.AWSRequest
    DescribeMediaStorageConfiguration
  where
  type
    AWSResponse DescribeMediaStorageConfiguration =
      DescribeMediaStorageConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMediaStorageConfigurationResponse'
            Prelude.<$> (x Data..?> "MediaStorageConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMediaStorageConfiguration
  where
  hashWithSalt
    _salt
    DescribeMediaStorageConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` channelARN
        `Prelude.hashWithSalt` channelName

instance
  Prelude.NFData
    DescribeMediaStorageConfiguration
  where
  rnf DescribeMediaStorageConfiguration' {..} =
    Prelude.rnf channelARN `Prelude.seq`
      Prelude.rnf channelName

instance
  Data.ToHeaders
    DescribeMediaStorageConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DescribeMediaStorageConfiguration
  where
  toJSON DescribeMediaStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelARN" Data..=) Prelude.<$> channelARN,
            ("ChannelName" Data..=) Prelude.<$> channelName
          ]
      )

instance
  Data.ToPath
    DescribeMediaStorageConfiguration
  where
  toPath =
    Prelude.const "/describeMediaStorageConfiguration"

instance
  Data.ToQuery
    DescribeMediaStorageConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMediaStorageConfigurationResponse' smart constructor.
data DescribeMediaStorageConfigurationResponse = DescribeMediaStorageConfigurationResponse'
  { -- | A structure that encapsulates, or contains, the media storage
    -- configuration properties.
    mediaStorageConfiguration :: Prelude.Maybe MediaStorageConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMediaStorageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaStorageConfiguration', 'describeMediaStorageConfigurationResponse_mediaStorageConfiguration' - A structure that encapsulates, or contains, the media storage
-- configuration properties.
--
-- 'httpStatus', 'describeMediaStorageConfigurationResponse_httpStatus' - The response's http status code.
newDescribeMediaStorageConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMediaStorageConfigurationResponse
newDescribeMediaStorageConfigurationResponse
  pHttpStatus_ =
    DescribeMediaStorageConfigurationResponse'
      { mediaStorageConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A structure that encapsulates, or contains, the media storage
-- configuration properties.
describeMediaStorageConfigurationResponse_mediaStorageConfiguration :: Lens.Lens' DescribeMediaStorageConfigurationResponse (Prelude.Maybe MediaStorageConfiguration)
describeMediaStorageConfigurationResponse_mediaStorageConfiguration = Lens.lens (\DescribeMediaStorageConfigurationResponse' {mediaStorageConfiguration} -> mediaStorageConfiguration) (\s@DescribeMediaStorageConfigurationResponse' {} a -> s {mediaStorageConfiguration = a} :: DescribeMediaStorageConfigurationResponse)

-- | The response's http status code.
describeMediaStorageConfigurationResponse_httpStatus :: Lens.Lens' DescribeMediaStorageConfigurationResponse Prelude.Int
describeMediaStorageConfigurationResponse_httpStatus = Lens.lens (\DescribeMediaStorageConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeMediaStorageConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeMediaStorageConfigurationResponse)

instance
  Prelude.NFData
    DescribeMediaStorageConfigurationResponse
  where
  rnf DescribeMediaStorageConfigurationResponse' {..} =
    Prelude.rnf mediaStorageConfiguration `Prelude.seq`
      Prelude.rnf httpStatus
