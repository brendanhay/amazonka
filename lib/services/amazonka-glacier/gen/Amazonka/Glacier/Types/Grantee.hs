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
-- Module      : Amazonka.Glacier.Types.Grantee
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.Grantee where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the grantee.
--
-- /See:/ 'newGrantee' smart constructor.
data Grantee = Grantee'
  { -- | Screen name of the grantee.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Email address of the grantee.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The canonical user ID of the grantee.
    id :: Prelude.Maybe Prelude.Text,
    -- | URI of the grantee group.
    uri :: Prelude.Maybe Prelude.Text,
    -- | Type of grantee
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Grantee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'grantee_displayName' - Screen name of the grantee.
--
-- 'emailAddress', 'grantee_emailAddress' - Email address of the grantee.
--
-- 'id', 'grantee_id' - The canonical user ID of the grantee.
--
-- 'uri', 'grantee_uri' - URI of the grantee group.
--
-- 'type'', 'grantee_type' - Type of grantee
newGrantee ::
  -- | 'type''
  Type ->
  Grantee
newGrantee pType_ =
  Grantee'
    { displayName = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      id = Prelude.Nothing,
      uri = Prelude.Nothing,
      type' = pType_
    }

-- | Screen name of the grantee.
grantee_displayName :: Lens.Lens' Grantee (Prelude.Maybe Prelude.Text)
grantee_displayName = Lens.lens (\Grantee' {displayName} -> displayName) (\s@Grantee' {} a -> s {displayName = a} :: Grantee)

-- | Email address of the grantee.
grantee_emailAddress :: Lens.Lens' Grantee (Prelude.Maybe Prelude.Text)
grantee_emailAddress = Lens.lens (\Grantee' {emailAddress} -> emailAddress) (\s@Grantee' {} a -> s {emailAddress = a} :: Grantee)

-- | The canonical user ID of the grantee.
grantee_id :: Lens.Lens' Grantee (Prelude.Maybe Prelude.Text)
grantee_id = Lens.lens (\Grantee' {id} -> id) (\s@Grantee' {} a -> s {id = a} :: Grantee)

-- | URI of the grantee group.
grantee_uri :: Lens.Lens' Grantee (Prelude.Maybe Prelude.Text)
grantee_uri = Lens.lens (\Grantee' {uri} -> uri) (\s@Grantee' {} a -> s {uri = a} :: Grantee)

-- | Type of grantee
grantee_type :: Lens.Lens' Grantee Type
grantee_type = Lens.lens (\Grantee' {type'} -> type') (\s@Grantee' {} a -> s {type' = a} :: Grantee)

instance Data.FromJSON Grantee where
  parseJSON =
    Data.withObject
      "Grantee"
      ( \x ->
          Grantee'
            Prelude.<$> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "ID")
            Prelude.<*> (x Data..:? "URI")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable Grantee where
  hashWithSalt _salt Grantee' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Grantee where
  rnf Grantee' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Grantee where
  toJSON Grantee' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("EmailAddress" Data..=) Prelude.<$> emailAddress,
            ("ID" Data..=) Prelude.<$> id,
            ("URI" Data..=) Prelude.<$> uri,
            Prelude.Just ("Type" Data..= type')
          ]
      )
