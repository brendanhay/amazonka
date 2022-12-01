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
-- Module      : Amazonka.MediaLive.Types.InputDestinationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDestinationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Endpoint settings for a PUSH type input.
--
-- /See:/ 'newInputDestinationRequest' smart constructor.
data InputDestinationRequest = InputDestinationRequest'
  { -- | A unique name for the location the RTMP stream is being pushed to.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDestinationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'inputDestinationRequest_streamName' - A unique name for the location the RTMP stream is being pushed to.
newInputDestinationRequest ::
  InputDestinationRequest
newInputDestinationRequest =
  InputDestinationRequest'
    { streamName =
        Prelude.Nothing
    }

-- | A unique name for the location the RTMP stream is being pushed to.
inputDestinationRequest_streamName :: Lens.Lens' InputDestinationRequest (Prelude.Maybe Prelude.Text)
inputDestinationRequest_streamName = Lens.lens (\InputDestinationRequest' {streamName} -> streamName) (\s@InputDestinationRequest' {} a -> s {streamName = a} :: InputDestinationRequest)

instance Prelude.Hashable InputDestinationRequest where
  hashWithSalt _salt InputDestinationRequest' {..} =
    _salt `Prelude.hashWithSalt` streamName

instance Prelude.NFData InputDestinationRequest where
  rnf InputDestinationRequest' {..} =
    Prelude.rnf streamName

instance Core.ToJSON InputDestinationRequest where
  toJSON InputDestinationRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [("streamName" Core..=) Prelude.<$> streamName]
      )
