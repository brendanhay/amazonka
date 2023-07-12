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
-- Module      : Amazonka.MacieV2.Types.SuppressDataIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SuppressDataIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.DataIdentifierType
import qualified Amazonka.Prelude as Prelude

-- | Specifies a custom data identifier or managed data identifier that
-- detected a type of sensitive data to start excluding or including in an
-- S3 bucket\'s sensitivity score.
--
-- /See:/ 'newSuppressDataIdentifier' smart constructor.
data SuppressDataIdentifier = SuppressDataIdentifier'
  { -- | The unique identifier for the custom data identifier or managed data
    -- identifier that detected the type of sensitive data to exclude or
    -- include in the score.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of data identifier that detected the sensitive data. Possible
    -- values are: CUSTOM, for a custom data identifier; and, MANAGED, for a
    -- managed data identifier.
    type' :: Prelude.Maybe DataIdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuppressDataIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'suppressDataIdentifier_id' - The unique identifier for the custom data identifier or managed data
-- identifier that detected the type of sensitive data to exclude or
-- include in the score.
--
-- 'type'', 'suppressDataIdentifier_type' - The type of data identifier that detected the sensitive data. Possible
-- values are: CUSTOM, for a custom data identifier; and, MANAGED, for a
-- managed data identifier.
newSuppressDataIdentifier ::
  SuppressDataIdentifier
newSuppressDataIdentifier =
  SuppressDataIdentifier'
    { id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The unique identifier for the custom data identifier or managed data
-- identifier that detected the type of sensitive data to exclude or
-- include in the score.
suppressDataIdentifier_id :: Lens.Lens' SuppressDataIdentifier (Prelude.Maybe Prelude.Text)
suppressDataIdentifier_id = Lens.lens (\SuppressDataIdentifier' {id} -> id) (\s@SuppressDataIdentifier' {} a -> s {id = a} :: SuppressDataIdentifier)

-- | The type of data identifier that detected the sensitive data. Possible
-- values are: CUSTOM, for a custom data identifier; and, MANAGED, for a
-- managed data identifier.
suppressDataIdentifier_type :: Lens.Lens' SuppressDataIdentifier (Prelude.Maybe DataIdentifierType)
suppressDataIdentifier_type = Lens.lens (\SuppressDataIdentifier' {type'} -> type') (\s@SuppressDataIdentifier' {} a -> s {type' = a} :: SuppressDataIdentifier)

instance Prelude.Hashable SuppressDataIdentifier where
  hashWithSalt _salt SuppressDataIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SuppressDataIdentifier where
  rnf SuppressDataIdentifier' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON SuppressDataIdentifier where
  toJSON SuppressDataIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("id" Data..=) Prelude.<$> id,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
