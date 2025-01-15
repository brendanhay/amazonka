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
-- Module      : Amazonka.MacieV2.Types.AllowListCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AllowListCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.S3WordsList
import qualified Amazonka.Prelude as Prelude

-- | Specifies the criteria for an allow list. The criteria must specify a
-- regular expression (regex) or an S3 object (s3WordsList). It can\'t
-- specify both.
--
-- /See:/ 'newAllowListCriteria' smart constructor.
data AllowListCriteria = AllowListCriteria'
  { -- | The regular expression (/regex/) that defines the text pattern to
    -- ignore. The expression can contain as many as 512 characters.
    regex :: Prelude.Maybe Prelude.Text,
    -- | The location and name of the S3 object that lists specific text to
    -- ignore.
    s3WordsList :: Prelude.Maybe S3WordsList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowListCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regex', 'allowListCriteria_regex' - The regular expression (/regex/) that defines the text pattern to
-- ignore. The expression can contain as many as 512 characters.
--
-- 's3WordsList', 'allowListCriteria_s3WordsList' - The location and name of the S3 object that lists specific text to
-- ignore.
newAllowListCriteria ::
  AllowListCriteria
newAllowListCriteria =
  AllowListCriteria'
    { regex = Prelude.Nothing,
      s3WordsList = Prelude.Nothing
    }

-- | The regular expression (/regex/) that defines the text pattern to
-- ignore. The expression can contain as many as 512 characters.
allowListCriteria_regex :: Lens.Lens' AllowListCriteria (Prelude.Maybe Prelude.Text)
allowListCriteria_regex = Lens.lens (\AllowListCriteria' {regex} -> regex) (\s@AllowListCriteria' {} a -> s {regex = a} :: AllowListCriteria)

-- | The location and name of the S3 object that lists specific text to
-- ignore.
allowListCriteria_s3WordsList :: Lens.Lens' AllowListCriteria (Prelude.Maybe S3WordsList)
allowListCriteria_s3WordsList = Lens.lens (\AllowListCriteria' {s3WordsList} -> s3WordsList) (\s@AllowListCriteria' {} a -> s {s3WordsList = a} :: AllowListCriteria)

instance Data.FromJSON AllowListCriteria where
  parseJSON =
    Data.withObject
      "AllowListCriteria"
      ( \x ->
          AllowListCriteria'
            Prelude.<$> (x Data..:? "regex")
            Prelude.<*> (x Data..:? "s3WordsList")
      )

instance Prelude.Hashable AllowListCriteria where
  hashWithSalt _salt AllowListCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` regex
      `Prelude.hashWithSalt` s3WordsList

instance Prelude.NFData AllowListCriteria where
  rnf AllowListCriteria' {..} =
    Prelude.rnf regex `Prelude.seq`
      Prelude.rnf s3WordsList

instance Data.ToJSON AllowListCriteria where
  toJSON AllowListCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("regex" Data..=) Prelude.<$> regex,
            ("s3WordsList" Data..=) Prelude.<$> s3WordsList
          ]
      )
