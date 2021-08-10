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
-- Module      : Network.AWS.SESv2.Types.SuppressedDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SuppressedDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.SuppressedDestinationAttributes
import Network.AWS.SESv2.Types.SuppressionListReason

-- | An object that contains information about an email address that is on
-- the suppression list for your account.
--
-- /See:/ 'newSuppressedDestination' smart constructor.
data SuppressedDestination = SuppressedDestination'
  { -- | An optional value that can contain additional information about the
    -- reasons that the address was added to the suppression list for your
    -- account.
    attributes :: Prelude.Maybe SuppressedDestinationAttributes,
    -- | The email address that is on the suppression list for your account.
    emailAddress :: Prelude.Text,
    -- | The reason that the address was added to the suppression list for your
    -- account.
    reason :: SuppressionListReason,
    -- | The date and time when the suppressed destination was last updated,
    -- shown in Unix time format.
    lastUpdateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuppressedDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'suppressedDestination_attributes' - An optional value that can contain additional information about the
-- reasons that the address was added to the suppression list for your
-- account.
--
-- 'emailAddress', 'suppressedDestination_emailAddress' - The email address that is on the suppression list for your account.
--
-- 'reason', 'suppressedDestination_reason' - The reason that the address was added to the suppression list for your
-- account.
--
-- 'lastUpdateTime', 'suppressedDestination_lastUpdateTime' - The date and time when the suppressed destination was last updated,
-- shown in Unix time format.
newSuppressedDestination ::
  -- | 'emailAddress'
  Prelude.Text ->
  -- | 'reason'
  SuppressionListReason ->
  -- | 'lastUpdateTime'
  Prelude.UTCTime ->
  SuppressedDestination
newSuppressedDestination
  pEmailAddress_
  pReason_
  pLastUpdateTime_ =
    SuppressedDestination'
      { attributes =
          Prelude.Nothing,
        emailAddress = pEmailAddress_,
        reason = pReason_,
        lastUpdateTime = Core._Time Lens.# pLastUpdateTime_
      }

-- | An optional value that can contain additional information about the
-- reasons that the address was added to the suppression list for your
-- account.
suppressedDestination_attributes :: Lens.Lens' SuppressedDestination (Prelude.Maybe SuppressedDestinationAttributes)
suppressedDestination_attributes = Lens.lens (\SuppressedDestination' {attributes} -> attributes) (\s@SuppressedDestination' {} a -> s {attributes = a} :: SuppressedDestination)

-- | The email address that is on the suppression list for your account.
suppressedDestination_emailAddress :: Lens.Lens' SuppressedDestination Prelude.Text
suppressedDestination_emailAddress = Lens.lens (\SuppressedDestination' {emailAddress} -> emailAddress) (\s@SuppressedDestination' {} a -> s {emailAddress = a} :: SuppressedDestination)

-- | The reason that the address was added to the suppression list for your
-- account.
suppressedDestination_reason :: Lens.Lens' SuppressedDestination SuppressionListReason
suppressedDestination_reason = Lens.lens (\SuppressedDestination' {reason} -> reason) (\s@SuppressedDestination' {} a -> s {reason = a} :: SuppressedDestination)

-- | The date and time when the suppressed destination was last updated,
-- shown in Unix time format.
suppressedDestination_lastUpdateTime :: Lens.Lens' SuppressedDestination Prelude.UTCTime
suppressedDestination_lastUpdateTime = Lens.lens (\SuppressedDestination' {lastUpdateTime} -> lastUpdateTime) (\s@SuppressedDestination' {} a -> s {lastUpdateTime = a} :: SuppressedDestination) Prelude.. Core._Time

instance Core.FromJSON SuppressedDestination where
  parseJSON =
    Core.withObject
      "SuppressedDestination"
      ( \x ->
          SuppressedDestination'
            Prelude.<$> (x Core..:? "Attributes")
            Prelude.<*> (x Core..: "EmailAddress")
            Prelude.<*> (x Core..: "Reason")
            Prelude.<*> (x Core..: "LastUpdateTime")
      )

instance Prelude.Hashable SuppressedDestination

instance Prelude.NFData SuppressedDestination
