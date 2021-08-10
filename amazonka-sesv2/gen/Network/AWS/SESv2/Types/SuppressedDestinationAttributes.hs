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
-- Module      : Network.AWS.SESv2.Types.SuppressedDestinationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SuppressedDestinationAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains additional attributes that are related an email
-- address that is on the suppression list for your account.
--
-- /See:/ 'newSuppressedDestinationAttributes' smart constructor.
data SuppressedDestinationAttributes = SuppressedDestinationAttributes'
  { -- | A unique identifier that\'s generated when an email address is added to
    -- the suppression list for your account.
    feedbackId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the email message that caused the email address
    -- to be added to the suppression list for your account.
    messageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuppressedDestinationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feedbackId', 'suppressedDestinationAttributes_feedbackId' - A unique identifier that\'s generated when an email address is added to
-- the suppression list for your account.
--
-- 'messageId', 'suppressedDestinationAttributes_messageId' - The unique identifier of the email message that caused the email address
-- to be added to the suppression list for your account.
newSuppressedDestinationAttributes ::
  SuppressedDestinationAttributes
newSuppressedDestinationAttributes =
  SuppressedDestinationAttributes'
    { feedbackId =
        Prelude.Nothing,
      messageId = Prelude.Nothing
    }

-- | A unique identifier that\'s generated when an email address is added to
-- the suppression list for your account.
suppressedDestinationAttributes_feedbackId :: Lens.Lens' SuppressedDestinationAttributes (Prelude.Maybe Prelude.Text)
suppressedDestinationAttributes_feedbackId = Lens.lens (\SuppressedDestinationAttributes' {feedbackId} -> feedbackId) (\s@SuppressedDestinationAttributes' {} a -> s {feedbackId = a} :: SuppressedDestinationAttributes)

-- | The unique identifier of the email message that caused the email address
-- to be added to the suppression list for your account.
suppressedDestinationAttributes_messageId :: Lens.Lens' SuppressedDestinationAttributes (Prelude.Maybe Prelude.Text)
suppressedDestinationAttributes_messageId = Lens.lens (\SuppressedDestinationAttributes' {messageId} -> messageId) (\s@SuppressedDestinationAttributes' {} a -> s {messageId = a} :: SuppressedDestinationAttributes)

instance
  Core.FromJSON
    SuppressedDestinationAttributes
  where
  parseJSON =
    Core.withObject
      "SuppressedDestinationAttributes"
      ( \x ->
          SuppressedDestinationAttributes'
            Prelude.<$> (x Core..:? "FeedbackId")
            Prelude.<*> (x Core..:? "MessageId")
      )

instance
  Prelude.Hashable
    SuppressedDestinationAttributes

instance
  Prelude.NFData
    SuppressedDestinationAttributes
