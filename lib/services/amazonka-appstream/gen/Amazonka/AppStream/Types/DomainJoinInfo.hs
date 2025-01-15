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
-- Module      : Amazonka.AppStream.Types.DomainJoinInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.DomainJoinInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration information required to join fleets and
-- image builders to Microsoft Active Directory domains.
--
-- /See:/ 'newDomainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { -- | The fully qualified name of the directory (for example,
    -- corp.example.com).
    directoryName :: Prelude.Maybe Prelude.Text,
    -- | The distinguished name of the organizational unit for computer accounts.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainJoinInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryName', 'domainJoinInfo_directoryName' - The fully qualified name of the directory (for example,
-- corp.example.com).
--
-- 'organizationalUnitDistinguishedName', 'domainJoinInfo_organizationalUnitDistinguishedName' - The distinguished name of the organizational unit for computer accounts.
newDomainJoinInfo ::
  DomainJoinInfo
newDomainJoinInfo =
  DomainJoinInfo'
    { directoryName = Prelude.Nothing,
      organizationalUnitDistinguishedName =
        Prelude.Nothing
    }

-- | The fully qualified name of the directory (for example,
-- corp.example.com).
domainJoinInfo_directoryName :: Lens.Lens' DomainJoinInfo (Prelude.Maybe Prelude.Text)
domainJoinInfo_directoryName = Lens.lens (\DomainJoinInfo' {directoryName} -> directoryName) (\s@DomainJoinInfo' {} a -> s {directoryName = a} :: DomainJoinInfo)

-- | The distinguished name of the organizational unit for computer accounts.
domainJoinInfo_organizationalUnitDistinguishedName :: Lens.Lens' DomainJoinInfo (Prelude.Maybe Prelude.Text)
domainJoinInfo_organizationalUnitDistinguishedName = Lens.lens (\DomainJoinInfo' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@DomainJoinInfo' {} a -> s {organizationalUnitDistinguishedName = a} :: DomainJoinInfo)

instance Data.FromJSON DomainJoinInfo where
  parseJSON =
    Data.withObject
      "DomainJoinInfo"
      ( \x ->
          DomainJoinInfo'
            Prelude.<$> (x Data..:? "DirectoryName")
            Prelude.<*> (x Data..:? "OrganizationalUnitDistinguishedName")
      )

instance Prelude.Hashable DomainJoinInfo where
  hashWithSalt _salt DomainJoinInfo' {..} =
    _salt
      `Prelude.hashWithSalt` directoryName
      `Prelude.hashWithSalt` organizationalUnitDistinguishedName

instance Prelude.NFData DomainJoinInfo where
  rnf DomainJoinInfo' {..} =
    Prelude.rnf directoryName `Prelude.seq`
      Prelude.rnf organizationalUnitDistinguishedName

instance Data.ToJSON DomainJoinInfo where
  toJSON DomainJoinInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryName" Data..=) Prelude.<$> directoryName,
            ("OrganizationalUnitDistinguishedName" Data..=)
              Prelude.<$> organizationalUnitDistinguishedName
          ]
      )
