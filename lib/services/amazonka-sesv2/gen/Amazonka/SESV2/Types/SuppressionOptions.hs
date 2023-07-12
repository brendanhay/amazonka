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
-- Module      : Amazonka.SESV2.Types.SuppressionOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.SuppressionOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.SuppressionListReason

-- | An object that contains information about the suppression list
-- preferences for your account.
--
-- /See:/ 'newSuppressionOptions' smart constructor.
data SuppressionOptions = SuppressionOptions'
  { -- | A list that contains the reasons that email addresses are automatically
    -- added to the suppression list for your account. This list can contain
    -- any or all of the following:
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
-- Create a value of 'SuppressionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressedReasons', 'suppressionOptions_suppressedReasons' - A list that contains the reasons that email addresses are automatically
-- added to the suppression list for your account. This list can contain
-- any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
newSuppressionOptions ::
  SuppressionOptions
newSuppressionOptions =
  SuppressionOptions'
    { suppressedReasons =
        Prelude.Nothing
    }

-- | A list that contains the reasons that email addresses are automatically
-- added to the suppression list for your account. This list can contain
-- any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
suppressionOptions_suppressedReasons :: Lens.Lens' SuppressionOptions (Prelude.Maybe [SuppressionListReason])
suppressionOptions_suppressedReasons = Lens.lens (\SuppressionOptions' {suppressedReasons} -> suppressedReasons) (\s@SuppressionOptions' {} a -> s {suppressedReasons = a} :: SuppressionOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SuppressionOptions where
  parseJSON =
    Data.withObject
      "SuppressionOptions"
      ( \x ->
          SuppressionOptions'
            Prelude.<$> ( x
                            Data..:? "SuppressedReasons"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SuppressionOptions where
  hashWithSalt _salt SuppressionOptions' {..} =
    _salt `Prelude.hashWithSalt` suppressedReasons

instance Prelude.NFData SuppressionOptions where
  rnf SuppressionOptions' {..} =
    Prelude.rnf suppressedReasons

instance Data.ToJSON SuppressionOptions where
  toJSON SuppressionOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SuppressedReasons" Data..=)
              Prelude.<$> suppressedReasons
          ]
      )
