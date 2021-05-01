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
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the SMS channel for an application.
--
-- /See:/ 'newSMSChannelRequest' smart constructor.
data SMSChannelRequest = SMSChannelRequest'
  { -- | Specifies whether to enable the SMS channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The registered short code that you want to use when you send messages
    -- through the SMS channel.
    shortCode :: Prelude.Maybe Prelude.Text,
    -- | The identity that you want to display on recipients\' devices when they
    -- receive messages from the SMS channel.
    senderId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SMSChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'sMSChannelRequest_enabled' - Specifies whether to enable the SMS channel for the application.
--
-- 'shortCode', 'sMSChannelRequest_shortCode' - The registered short code that you want to use when you send messages
-- through the SMS channel.
--
-- 'senderId', 'sMSChannelRequest_senderId' - The identity that you want to display on recipients\' devices when they
-- receive messages from the SMS channel.
newSMSChannelRequest ::
  SMSChannelRequest
newSMSChannelRequest =
  SMSChannelRequest'
    { enabled = Prelude.Nothing,
      shortCode = Prelude.Nothing,
      senderId = Prelude.Nothing
    }

-- | Specifies whether to enable the SMS channel for the application.
sMSChannelRequest_enabled :: Lens.Lens' SMSChannelRequest (Prelude.Maybe Prelude.Bool)
sMSChannelRequest_enabled = Lens.lens (\SMSChannelRequest' {enabled} -> enabled) (\s@SMSChannelRequest' {} a -> s {enabled = a} :: SMSChannelRequest)

-- | The registered short code that you want to use when you send messages
-- through the SMS channel.
sMSChannelRequest_shortCode :: Lens.Lens' SMSChannelRequest (Prelude.Maybe Prelude.Text)
sMSChannelRequest_shortCode = Lens.lens (\SMSChannelRequest' {shortCode} -> shortCode) (\s@SMSChannelRequest' {} a -> s {shortCode = a} :: SMSChannelRequest)

-- | The identity that you want to display on recipients\' devices when they
-- receive messages from the SMS channel.
sMSChannelRequest_senderId :: Lens.Lens' SMSChannelRequest (Prelude.Maybe Prelude.Text)
sMSChannelRequest_senderId = Lens.lens (\SMSChannelRequest' {senderId} -> senderId) (\s@SMSChannelRequest' {} a -> s {senderId = a} :: SMSChannelRequest)

instance Prelude.Hashable SMSChannelRequest

instance Prelude.NFData SMSChannelRequest

instance Prelude.ToJSON SMSChannelRequest where
  toJSON SMSChannelRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("ShortCode" Prelude..=) Prelude.<$> shortCode,
            ("SenderId" Prelude..=) Prelude.<$> senderId
          ]
      )
