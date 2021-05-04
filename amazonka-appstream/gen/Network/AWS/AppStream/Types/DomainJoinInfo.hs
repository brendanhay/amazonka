{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.Types.DomainJoinInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.DomainJoinInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration information required to join fleets and
-- image builders to Microsoft Active Directory domains.
--
-- /See:/ 'newDomainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { -- | The distinguished name of the organizational unit for computer accounts.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified name of the directory (for example,
    -- corp.example.com).
    directoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DomainJoinInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitDistinguishedName', 'domainJoinInfo_organizationalUnitDistinguishedName' - The distinguished name of the organizational unit for computer accounts.
--
-- 'directoryName', 'domainJoinInfo_directoryName' - The fully qualified name of the directory (for example,
-- corp.example.com).
newDomainJoinInfo ::
  DomainJoinInfo
newDomainJoinInfo =
  DomainJoinInfo'
    { organizationalUnitDistinguishedName =
        Prelude.Nothing,
      directoryName = Prelude.Nothing
    }

-- | The distinguished name of the organizational unit for computer accounts.
domainJoinInfo_organizationalUnitDistinguishedName :: Lens.Lens' DomainJoinInfo (Prelude.Maybe Prelude.Text)
domainJoinInfo_organizationalUnitDistinguishedName = Lens.lens (\DomainJoinInfo' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@DomainJoinInfo' {} a -> s {organizationalUnitDistinguishedName = a} :: DomainJoinInfo)

-- | The fully qualified name of the directory (for example,
-- corp.example.com).
domainJoinInfo_directoryName :: Lens.Lens' DomainJoinInfo (Prelude.Maybe Prelude.Text)
domainJoinInfo_directoryName = Lens.lens (\DomainJoinInfo' {directoryName} -> directoryName) (\s@DomainJoinInfo' {} a -> s {directoryName = a} :: DomainJoinInfo)

instance Prelude.FromJSON DomainJoinInfo where
  parseJSON =
    Prelude.withObject
      "DomainJoinInfo"
      ( \x ->
          DomainJoinInfo'
            Prelude.<$> (x Prelude..:? "OrganizationalUnitDistinguishedName")
            Prelude.<*> (x Prelude..:? "DirectoryName")
      )

instance Prelude.Hashable DomainJoinInfo

instance Prelude.NFData DomainJoinInfo

instance Prelude.ToJSON DomainJoinInfo where
  toJSON DomainJoinInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnitDistinguishedName" Prelude..=)
              Prelude.<$> organizationalUnitDistinguishedName,
            ("DirectoryName" Prelude..=)
              Prelude.<$> directoryName
          ]
      )
