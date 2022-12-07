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
-- Module      : Amazonka.FSx.Types.ActiveDirectoryBackupAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.ActiveDirectoryBackupAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Microsoft Active Directory attributes of the Amazon FSx for Windows
-- File Server file system.
--
-- /See:/ 'newActiveDirectoryBackupAttributes' smart constructor.
data ActiveDirectoryBackupAttributes = ActiveDirectoryBackupAttributes'
  { -- | The ID of the Amazon Web Services Managed Microsoft Active Directory
    -- instance to which the file system is joined.
    activeDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name of the self-managed Active Directory
    -- directory.
    domainName :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveDirectoryBackupAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryId', 'activeDirectoryBackupAttributes_activeDirectoryId' - The ID of the Amazon Web Services Managed Microsoft Active Directory
-- instance to which the file system is joined.
--
-- 'domainName', 'activeDirectoryBackupAttributes_domainName' - The fully qualified domain name of the self-managed Active Directory
-- directory.
--
-- 'resourceARN', 'activeDirectoryBackupAttributes_resourceARN' - Undocumented member.
newActiveDirectoryBackupAttributes ::
  ActiveDirectoryBackupAttributes
newActiveDirectoryBackupAttributes =
  ActiveDirectoryBackupAttributes'
    { activeDirectoryId =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      resourceARN = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services Managed Microsoft Active Directory
-- instance to which the file system is joined.
activeDirectoryBackupAttributes_activeDirectoryId :: Lens.Lens' ActiveDirectoryBackupAttributes (Prelude.Maybe Prelude.Text)
activeDirectoryBackupAttributes_activeDirectoryId = Lens.lens (\ActiveDirectoryBackupAttributes' {activeDirectoryId} -> activeDirectoryId) (\s@ActiveDirectoryBackupAttributes' {} a -> s {activeDirectoryId = a} :: ActiveDirectoryBackupAttributes)

-- | The fully qualified domain name of the self-managed Active Directory
-- directory.
activeDirectoryBackupAttributes_domainName :: Lens.Lens' ActiveDirectoryBackupAttributes (Prelude.Maybe Prelude.Text)
activeDirectoryBackupAttributes_domainName = Lens.lens (\ActiveDirectoryBackupAttributes' {domainName} -> domainName) (\s@ActiveDirectoryBackupAttributes' {} a -> s {domainName = a} :: ActiveDirectoryBackupAttributes)

-- | Undocumented member.
activeDirectoryBackupAttributes_resourceARN :: Lens.Lens' ActiveDirectoryBackupAttributes (Prelude.Maybe Prelude.Text)
activeDirectoryBackupAttributes_resourceARN = Lens.lens (\ActiveDirectoryBackupAttributes' {resourceARN} -> resourceARN) (\s@ActiveDirectoryBackupAttributes' {} a -> s {resourceARN = a} :: ActiveDirectoryBackupAttributes)

instance
  Data.FromJSON
    ActiveDirectoryBackupAttributes
  where
  parseJSON =
    Data.withObject
      "ActiveDirectoryBackupAttributes"
      ( \x ->
          ActiveDirectoryBackupAttributes'
            Prelude.<$> (x Data..:? "ActiveDirectoryId")
            Prelude.<*> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "ResourceARN")
      )

instance
  Prelude.Hashable
    ActiveDirectoryBackupAttributes
  where
  hashWithSalt
    _salt
    ActiveDirectoryBackupAttributes' {..} =
      _salt `Prelude.hashWithSalt` activeDirectoryId
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` resourceARN

instance
  Prelude.NFData
    ActiveDirectoryBackupAttributes
  where
  rnf ActiveDirectoryBackupAttributes' {..} =
    Prelude.rnf activeDirectoryId
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf resourceARN
