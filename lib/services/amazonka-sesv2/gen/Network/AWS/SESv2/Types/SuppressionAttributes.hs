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
-- Module      : Network.AWS.SESv2.Types.SuppressionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SuppressionAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.SuppressionListReason

-- | An object that contains information about the email address suppression
-- preferences for your account in the current Amazon Web Services Region.
--
-- /See:/ 'newSuppressionAttributes' smart constructor.
data SuppressionAttributes = SuppressionAttributes'
  { -- | A list that contains the reasons that email addresses will be
    -- automatically added to the suppression list for your account. This list
    -- can contain any or all of the following:
    --
    -- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
    --     list for your account when a message sent to that address results in
    --     a complaint.
    --
    -- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
    --     for your account when a message sent to that address results in a
    --     hard bounce.
    suppressedReasons :: Prelude.Maybe [SuppressionListReason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuppressionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressedReasons', 'suppressionAttributes_suppressedReasons' - A list that contains the reasons that email addresses will be
-- automatically added to the suppression list for your account. This list
-- can contain any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
newSuppressionAttributes ::
  SuppressionAttributes
newSuppressionAttributes =
  SuppressionAttributes'
    { suppressedReasons =
        Prelude.Nothing
    }

-- | A list that contains the reasons that email addresses will be
-- automatically added to the suppression list for your account. This list
-- can contain any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
suppressionAttributes_suppressedReasons :: Lens.Lens' SuppressionAttributes (Prelude.Maybe [SuppressionListReason])
suppressionAttributes_suppressedReasons = Lens.lens (\SuppressionAttributes' {suppressedReasons} -> suppressedReasons) (\s@SuppressionAttributes' {} a -> s {suppressedReasons = a} :: SuppressionAttributes) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SuppressionAttributes where
  parseJSON =
    Core.withObject
      "SuppressionAttributes"
      ( \x ->
          SuppressionAttributes'
            Prelude.<$> ( x Core..:? "SuppressedReasons"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SuppressionAttributes

instance Prelude.NFData SuppressionAttributes
