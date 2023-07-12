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
-- Module      : Amazonka.CustomerProfiles.Types.ObjectTypeKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ObjectTypeKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.StandardIdentifier
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines the Key element of a ProfileObject. A Key is a
-- special element that can be used to search for a customer profile.
--
-- /See:/ 'newObjectTypeKey' smart constructor.
data ObjectTypeKey = ObjectTypeKey'
  { -- | The reference for the key name of the fields map.
    fieldNames :: Prelude.Maybe [Prelude.Text],
    -- | The types of keys that a ProfileObject can have. Each ProfileObject can
    -- have only 1 UNIQUE key but multiple PROFILE keys. PROFILE, ASSET, CASE,
    -- or ORDER means that this key can be used to tie an object to a PROFILE,
    -- ASSET, CASE, or ORDER respectively. UNIQUE means that it can be used to
    -- uniquely identify an object. If a key a is marked as SECONDARY, it will
    -- be used to search for profiles after all other PROFILE keys have been
    -- searched. A LOOKUP_ONLY key is only used to match a profile but is not
    -- persisted to be used for searching of the profile. A NEW_ONLY key is
    -- only used if the profile does not already exist before the object is
    -- ingested, otherwise it is only used for matching objects to profiles.
    standardIdentifiers :: Prelude.Maybe [StandardIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectTypeKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldNames', 'objectTypeKey_fieldNames' - The reference for the key name of the fields map.
--
-- 'standardIdentifiers', 'objectTypeKey_standardIdentifiers' - The types of keys that a ProfileObject can have. Each ProfileObject can
-- have only 1 UNIQUE key but multiple PROFILE keys. PROFILE, ASSET, CASE,
-- or ORDER means that this key can be used to tie an object to a PROFILE,
-- ASSET, CASE, or ORDER respectively. UNIQUE means that it can be used to
-- uniquely identify an object. If a key a is marked as SECONDARY, it will
-- be used to search for profiles after all other PROFILE keys have been
-- searched. A LOOKUP_ONLY key is only used to match a profile but is not
-- persisted to be used for searching of the profile. A NEW_ONLY key is
-- only used if the profile does not already exist before the object is
-- ingested, otherwise it is only used for matching objects to profiles.
newObjectTypeKey ::
  ObjectTypeKey
newObjectTypeKey =
  ObjectTypeKey'
    { fieldNames = Prelude.Nothing,
      standardIdentifiers = Prelude.Nothing
    }

-- | The reference for the key name of the fields map.
objectTypeKey_fieldNames :: Lens.Lens' ObjectTypeKey (Prelude.Maybe [Prelude.Text])
objectTypeKey_fieldNames = Lens.lens (\ObjectTypeKey' {fieldNames} -> fieldNames) (\s@ObjectTypeKey' {} a -> s {fieldNames = a} :: ObjectTypeKey) Prelude.. Lens.mapping Lens.coerced

-- | The types of keys that a ProfileObject can have. Each ProfileObject can
-- have only 1 UNIQUE key but multiple PROFILE keys. PROFILE, ASSET, CASE,
-- or ORDER means that this key can be used to tie an object to a PROFILE,
-- ASSET, CASE, or ORDER respectively. UNIQUE means that it can be used to
-- uniquely identify an object. If a key a is marked as SECONDARY, it will
-- be used to search for profiles after all other PROFILE keys have been
-- searched. A LOOKUP_ONLY key is only used to match a profile but is not
-- persisted to be used for searching of the profile. A NEW_ONLY key is
-- only used if the profile does not already exist before the object is
-- ingested, otherwise it is only used for matching objects to profiles.
objectTypeKey_standardIdentifiers :: Lens.Lens' ObjectTypeKey (Prelude.Maybe [StandardIdentifier])
objectTypeKey_standardIdentifiers = Lens.lens (\ObjectTypeKey' {standardIdentifiers} -> standardIdentifiers) (\s@ObjectTypeKey' {} a -> s {standardIdentifiers = a} :: ObjectTypeKey) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ObjectTypeKey where
  parseJSON =
    Data.withObject
      "ObjectTypeKey"
      ( \x ->
          ObjectTypeKey'
            Prelude.<$> (x Data..:? "FieldNames" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "StandardIdentifiers"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ObjectTypeKey where
  hashWithSalt _salt ObjectTypeKey' {..} =
    _salt
      `Prelude.hashWithSalt` fieldNames
      `Prelude.hashWithSalt` standardIdentifiers

instance Prelude.NFData ObjectTypeKey where
  rnf ObjectTypeKey' {..} =
    Prelude.rnf fieldNames
      `Prelude.seq` Prelude.rnf standardIdentifiers

instance Data.ToJSON ObjectTypeKey where
  toJSON ObjectTypeKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldNames" Data..=) Prelude.<$> fieldNames,
            ("StandardIdentifiers" Data..=)
              Prelude.<$> standardIdentifiers
          ]
      )
