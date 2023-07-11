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
-- Module      : Amazonka.EC2.Types.DirectoryServiceAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DirectoryServiceAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an Active Directory.
--
-- /See:/ 'newDirectoryServiceAuthentication' smart constructor.
data DirectoryServiceAuthentication = DirectoryServiceAuthentication'
  { -- | The ID of the Active Directory used for authentication.
    directoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectoryServiceAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'directoryServiceAuthentication_directoryId' - The ID of the Active Directory used for authentication.
newDirectoryServiceAuthentication ::
  DirectoryServiceAuthentication
newDirectoryServiceAuthentication =
  DirectoryServiceAuthentication'
    { directoryId =
        Prelude.Nothing
    }

-- | The ID of the Active Directory used for authentication.
directoryServiceAuthentication_directoryId :: Lens.Lens' DirectoryServiceAuthentication (Prelude.Maybe Prelude.Text)
directoryServiceAuthentication_directoryId = Lens.lens (\DirectoryServiceAuthentication' {directoryId} -> directoryId) (\s@DirectoryServiceAuthentication' {} a -> s {directoryId = a} :: DirectoryServiceAuthentication)

instance Data.FromXML DirectoryServiceAuthentication where
  parseXML x =
    DirectoryServiceAuthentication'
      Prelude.<$> (x Data..@? "directoryId")

instance
  Prelude.Hashable
    DirectoryServiceAuthentication
  where
  hashWithSalt
    _salt
    DirectoryServiceAuthentication' {..} =
      _salt `Prelude.hashWithSalt` directoryId

instance
  Prelude.NFData
    DirectoryServiceAuthentication
  where
  rnf DirectoryServiceAuthentication' {..} =
    Prelude.rnf directoryId
