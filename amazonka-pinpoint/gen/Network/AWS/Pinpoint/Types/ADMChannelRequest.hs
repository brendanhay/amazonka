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
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the ADM (Amazon Device Messaging)
-- channel for an application.
--
-- /See:/ 'newADMChannelRequest' smart constructor.
data ADMChannelRequest = ADMChannelRequest'
  { -- | Specifies whether to enable the ADM channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Client Secret that you received from Amazon to send messages by
    -- using ADM.
    clientSecret :: Prelude.Text,
    -- | The Client ID that you received from Amazon to send messages by using
    -- ADM.
    clientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ADMChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'aDMChannelRequest_enabled' - Specifies whether to enable the ADM channel for the application.
--
-- 'clientSecret', 'aDMChannelRequest_clientSecret' - The Client Secret that you received from Amazon to send messages by
-- using ADM.
--
-- 'clientId', 'aDMChannelRequest_clientId' - The Client ID that you received from Amazon to send messages by using
-- ADM.
newADMChannelRequest ::
  -- | 'clientSecret'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  ADMChannelRequest
newADMChannelRequest pClientSecret_ pClientId_ =
  ADMChannelRequest'
    { enabled = Prelude.Nothing,
      clientSecret = pClientSecret_,
      clientId = pClientId_
    }

-- | Specifies whether to enable the ADM channel for the application.
aDMChannelRequest_enabled :: Lens.Lens' ADMChannelRequest (Prelude.Maybe Prelude.Bool)
aDMChannelRequest_enabled = Lens.lens (\ADMChannelRequest' {enabled} -> enabled) (\s@ADMChannelRequest' {} a -> s {enabled = a} :: ADMChannelRequest)

-- | The Client Secret that you received from Amazon to send messages by
-- using ADM.
aDMChannelRequest_clientSecret :: Lens.Lens' ADMChannelRequest Prelude.Text
aDMChannelRequest_clientSecret = Lens.lens (\ADMChannelRequest' {clientSecret} -> clientSecret) (\s@ADMChannelRequest' {} a -> s {clientSecret = a} :: ADMChannelRequest)

-- | The Client ID that you received from Amazon to send messages by using
-- ADM.
aDMChannelRequest_clientId :: Lens.Lens' ADMChannelRequest Prelude.Text
aDMChannelRequest_clientId = Lens.lens (\ADMChannelRequest' {clientId} -> clientId) (\s@ADMChannelRequest' {} a -> s {clientId = a} :: ADMChannelRequest)

instance Prelude.Hashable ADMChannelRequest

instance Prelude.NFData ADMChannelRequest

instance Prelude.ToJSON ADMChannelRequest where
  toJSON ADMChannelRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            Prelude.Just
              ("ClientSecret" Prelude..= clientSecret),
            Prelude.Just ("ClientId" Prelude..= clientId)
          ]
      )
