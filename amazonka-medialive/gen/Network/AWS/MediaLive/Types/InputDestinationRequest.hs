{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.InputDestinationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Endpoint settings for a PUSH type input.
--
-- /See:/ 'newInputDestinationRequest' smart constructor.
data InputDestinationRequest = InputDestinationRequest'
  { -- | A unique name for the location the RTMP stream is being pushed to.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable InputDestinationRequest

instance Prelude.NFData InputDestinationRequest

instance Prelude.ToJSON InputDestinationRequest where
  toJSON InputDestinationRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("streamName" Prelude..=) Prelude.<$> streamName]
      )
