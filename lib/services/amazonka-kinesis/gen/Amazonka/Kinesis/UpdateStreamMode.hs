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
-- Module      : Amazonka.Kinesis.UpdateStreamMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the capacity mode of the data stream. Currently, in Kinesis Data
-- Streams, you can choose between an __on-demand__ capacity mode and a
-- __provisioned__ capacity mode for your data stream.
module Amazonka.Kinesis.UpdateStreamMode
  ( -- * Creating a Request
    UpdateStreamMode (..),
    newUpdateStreamMode,

    -- * Request Lenses
    updateStreamMode_streamARN,
    updateStreamMode_streamModeDetails,

    -- * Destructuring the Response
    UpdateStreamModeResponse (..),
    newUpdateStreamModeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStreamMode' smart constructor.
data UpdateStreamMode = UpdateStreamMode'
  { -- | Specifies the ARN of the data stream whose capacity mode you want to
    -- update.
    streamARN :: Prelude.Text,
    -- | Specifies the capacity mode to which you want to set your data stream.
    -- Currently, in Kinesis Data Streams, you can choose between an
    -- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
    -- data streams.
    streamModeDetails :: StreamModeDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamMode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'updateStreamMode_streamARN' - Specifies the ARN of the data stream whose capacity mode you want to
-- update.
--
-- 'streamModeDetails', 'updateStreamMode_streamModeDetails' - Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
newUpdateStreamMode ::
  -- | 'streamARN'
  Prelude.Text ->
  -- | 'streamModeDetails'
  StreamModeDetails ->
  UpdateStreamMode
newUpdateStreamMode pStreamARN_ pStreamModeDetails_ =
  UpdateStreamMode'
    { streamARN = pStreamARN_,
      streamModeDetails = pStreamModeDetails_
    }

-- | Specifies the ARN of the data stream whose capacity mode you want to
-- update.
updateStreamMode_streamARN :: Lens.Lens' UpdateStreamMode Prelude.Text
updateStreamMode_streamARN = Lens.lens (\UpdateStreamMode' {streamARN} -> streamARN) (\s@UpdateStreamMode' {} a -> s {streamARN = a} :: UpdateStreamMode)

-- | Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
updateStreamMode_streamModeDetails :: Lens.Lens' UpdateStreamMode StreamModeDetails
updateStreamMode_streamModeDetails = Lens.lens (\UpdateStreamMode' {streamModeDetails} -> streamModeDetails) (\s@UpdateStreamMode' {} a -> s {streamModeDetails = a} :: UpdateStreamMode)

instance Core.AWSRequest UpdateStreamMode where
  type
    AWSResponse UpdateStreamMode =
      UpdateStreamModeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateStreamModeResponse'

instance Prelude.Hashable UpdateStreamMode where
  hashWithSalt _salt UpdateStreamMode' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamModeDetails

instance Prelude.NFData UpdateStreamMode where
  rnf UpdateStreamMode' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamModeDetails

instance Data.ToHeaders UpdateStreamMode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.UpdateStreamMode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStreamMode where
  toJSON UpdateStreamMode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamARN" Data..= streamARN),
            Prelude.Just
              ("StreamModeDetails" Data..= streamModeDetails)
          ]
      )

instance Data.ToPath UpdateStreamMode where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStreamMode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStreamModeResponse' smart constructor.
data UpdateStreamModeResponse = UpdateStreamModeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamModeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateStreamModeResponse ::
  UpdateStreamModeResponse
newUpdateStreamModeResponse =
  UpdateStreamModeResponse'

instance Prelude.NFData UpdateStreamModeResponse where
  rnf _ = ()
