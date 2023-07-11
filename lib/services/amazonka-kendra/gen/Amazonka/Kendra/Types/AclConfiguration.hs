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
-- Module      : Amazonka.Kendra.Types.AclConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AclConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the column that should be used for filtering
-- the query response by groups.
--
-- /See:/ 'newAclConfiguration' smart constructor.
data AclConfiguration = AclConfiguration'
  { -- | A list of groups, separated by semi-colons, that filters a query
    -- response based on user context. The document is only returned to users
    -- that are in one of the groups specified in the @UserContext@ field of
    -- the @Query@ API.
    allowedGroupsColumnName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AclConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedGroupsColumnName', 'aclConfiguration_allowedGroupsColumnName' - A list of groups, separated by semi-colons, that filters a query
-- response based on user context. The document is only returned to users
-- that are in one of the groups specified in the @UserContext@ field of
-- the @Query@ API.
newAclConfiguration ::
  -- | 'allowedGroupsColumnName'
  Prelude.Text ->
  AclConfiguration
newAclConfiguration pAllowedGroupsColumnName_ =
  AclConfiguration'
    { allowedGroupsColumnName =
        pAllowedGroupsColumnName_
    }

-- | A list of groups, separated by semi-colons, that filters a query
-- response based on user context. The document is only returned to users
-- that are in one of the groups specified in the @UserContext@ field of
-- the @Query@ API.
aclConfiguration_allowedGroupsColumnName :: Lens.Lens' AclConfiguration Prelude.Text
aclConfiguration_allowedGroupsColumnName = Lens.lens (\AclConfiguration' {allowedGroupsColumnName} -> allowedGroupsColumnName) (\s@AclConfiguration' {} a -> s {allowedGroupsColumnName = a} :: AclConfiguration)

instance Data.FromJSON AclConfiguration where
  parseJSON =
    Data.withObject
      "AclConfiguration"
      ( \x ->
          AclConfiguration'
            Prelude.<$> (x Data..: "AllowedGroupsColumnName")
      )

instance Prelude.Hashable AclConfiguration where
  hashWithSalt _salt AclConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` allowedGroupsColumnName

instance Prelude.NFData AclConfiguration where
  rnf AclConfiguration' {..} =
    Prelude.rnf allowedGroupsColumnName

instance Data.ToJSON AclConfiguration where
  toJSON AclConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AllowedGroupsColumnName"
                  Data..= allowedGroupsColumnName
              )
          ]
      )
