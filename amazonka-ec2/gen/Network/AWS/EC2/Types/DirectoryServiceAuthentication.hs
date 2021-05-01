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
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DirectoryServiceAuthentication where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Active Directory.
--
-- /See:/ 'newDirectoryServiceAuthentication' smart constructor.
data DirectoryServiceAuthentication = DirectoryServiceAuthentication'
  { -- | The ID of the Active Directory used for authentication.
    directoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromXML
    DirectoryServiceAuthentication
  where
  parseXML x =
    DirectoryServiceAuthentication'
      Prelude.<$> (x Prelude..@? "directoryId")

instance
  Prelude.Hashable
    DirectoryServiceAuthentication

instance
  Prelude.NFData
    DirectoryServiceAuthentication
