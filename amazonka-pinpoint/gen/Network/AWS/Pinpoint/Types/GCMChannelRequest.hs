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
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the GCM channel for an application.
-- This channel enables Amazon Pinpoint to send push notifications through
-- the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging
-- (GCM), service.
--
-- /See:/ 'newGCMChannelRequest' smart constructor.
data GCMChannelRequest = GCMChannelRequest'
  { -- | Specifies whether to enable the GCM channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Web API Key, also referred to as an /API_KEY/ or /server key/, that
    -- you received from Google to communicate with Google services.
    apiKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GCMChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'gCMChannelRequest_enabled' - Specifies whether to enable the GCM channel for the application.
--
-- 'apiKey', 'gCMChannelRequest_apiKey' - The Web API Key, also referred to as an /API_KEY/ or /server key/, that
-- you received from Google to communicate with Google services.
newGCMChannelRequest ::
  -- | 'apiKey'
  Prelude.Text ->
  GCMChannelRequest
newGCMChannelRequest pApiKey_ =
  GCMChannelRequest'
    { enabled = Prelude.Nothing,
      apiKey = pApiKey_
    }

-- | Specifies whether to enable the GCM channel for the application.
gCMChannelRequest_enabled :: Lens.Lens' GCMChannelRequest (Prelude.Maybe Prelude.Bool)
gCMChannelRequest_enabled = Lens.lens (\GCMChannelRequest' {enabled} -> enabled) (\s@GCMChannelRequest' {} a -> s {enabled = a} :: GCMChannelRequest)

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/, that
-- you received from Google to communicate with Google services.
gCMChannelRequest_apiKey :: Lens.Lens' GCMChannelRequest Prelude.Text
gCMChannelRequest_apiKey = Lens.lens (\GCMChannelRequest' {apiKey} -> apiKey) (\s@GCMChannelRequest' {} a -> s {apiKey = a} :: GCMChannelRequest)

instance Prelude.Hashable GCMChannelRequest

instance Prelude.NFData GCMChannelRequest

instance Prelude.ToJSON GCMChannelRequest where
  toJSON GCMChannelRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            Prelude.Just ("ApiKey" Prelude..= apiKey)
          ]
      )
