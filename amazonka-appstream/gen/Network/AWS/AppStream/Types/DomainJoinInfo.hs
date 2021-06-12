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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration information required to join fleets and
-- image builders to Microsoft Active Directory domains.
--
-- /See:/ 'newDomainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { -- | The distinguished name of the organizational unit for computer accounts.
    organizationalUnitDistinguishedName :: Core.Maybe Core.Text,
    -- | The fully qualified name of the directory (for example,
    -- corp.example.com).
    directoryName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      directoryName = Core.Nothing
    }

-- | The distinguished name of the organizational unit for computer accounts.
domainJoinInfo_organizationalUnitDistinguishedName :: Lens.Lens' DomainJoinInfo (Core.Maybe Core.Text)
domainJoinInfo_organizationalUnitDistinguishedName = Lens.lens (\DomainJoinInfo' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@DomainJoinInfo' {} a -> s {organizationalUnitDistinguishedName = a} :: DomainJoinInfo)

-- | The fully qualified name of the directory (for example,
-- corp.example.com).
domainJoinInfo_directoryName :: Lens.Lens' DomainJoinInfo (Core.Maybe Core.Text)
domainJoinInfo_directoryName = Lens.lens (\DomainJoinInfo' {directoryName} -> directoryName) (\s@DomainJoinInfo' {} a -> s {directoryName = a} :: DomainJoinInfo)

instance Core.FromJSON DomainJoinInfo where
  parseJSON =
    Core.withObject
      "DomainJoinInfo"
      ( \x ->
          DomainJoinInfo'
            Core.<$> (x Core..:? "OrganizationalUnitDistinguishedName")
            Core.<*> (x Core..:? "DirectoryName")
      )

instance Core.Hashable DomainJoinInfo

instance Core.NFData DomainJoinInfo

instance Core.ToJSON DomainJoinInfo where
  toJSON DomainJoinInfo' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OrganizationalUnitDistinguishedName" Core..=)
              Core.<$> organizationalUnitDistinguishedName,
            ("DirectoryName" Core..=) Core.<$> directoryName
          ]
      )
