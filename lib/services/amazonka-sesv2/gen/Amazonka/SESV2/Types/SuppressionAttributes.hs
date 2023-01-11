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
-- Module      : Amazonka.SESV2.Types.SuppressionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.SuppressionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.SuppressionListReason

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

instance Data.FromJSON SuppressionAttributes where
  parseJSON =
    Data.withObject
      "SuppressionAttributes"
      ( \x ->
          SuppressionAttributes'
            Prelude.<$> ( x Data..:? "SuppressedReasons"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SuppressionAttributes where
  hashWithSalt _salt SuppressionAttributes' {..} =
    _salt `Prelude.hashWithSalt` suppressedReasons

instance Prelude.NFData SuppressionAttributes where
  rnf SuppressionAttributes' {..} =
    Prelude.rnf suppressedReasons
