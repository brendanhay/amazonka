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
-- Module      : Amazonka.EC2.Types.PrefixListId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrefixListId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a prefix list ID.
--
-- /See:/ 'newPrefixListId' smart constructor.
data PrefixListId = PrefixListId'
  { -- | A description for the security group rule that references this prefix
    -- list ID.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix.
    prefixListId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrefixListId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'prefixListId_description' - A description for the security group rule that references this prefix
-- list ID.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
--
-- 'prefixListId', 'prefixListId_prefixListId' - The ID of the prefix.
newPrefixListId ::
  PrefixListId
newPrefixListId =
  PrefixListId'
    { description = Prelude.Nothing,
      prefixListId = Prelude.Nothing
    }

-- | A description for the security group rule that references this prefix
-- list ID.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
prefixListId_description :: Lens.Lens' PrefixListId (Prelude.Maybe Prelude.Text)
prefixListId_description = Lens.lens (\PrefixListId' {description} -> description) (\s@PrefixListId' {} a -> s {description = a} :: PrefixListId)

-- | The ID of the prefix.
prefixListId_prefixListId :: Lens.Lens' PrefixListId (Prelude.Maybe Prelude.Text)
prefixListId_prefixListId = Lens.lens (\PrefixListId' {prefixListId} -> prefixListId) (\s@PrefixListId' {} a -> s {prefixListId = a} :: PrefixListId)

instance Data.FromXML PrefixListId where
  parseXML x =
    PrefixListId'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "prefixListId")

instance Prelude.Hashable PrefixListId where
  hashWithSalt _salt PrefixListId' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` prefixListId

instance Prelude.NFData PrefixListId where
  rnf PrefixListId' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf prefixListId

instance Data.ToQuery PrefixListId where
  toQuery PrefixListId' {..} =
    Prelude.mconcat
      [ "Description" Data.=: description,
        "PrefixListId" Data.=: prefixListId
      ]
