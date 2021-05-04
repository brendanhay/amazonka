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
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Active Directory to be used for client authentication.
--
-- /See:/ 'newDirectoryServiceAuthenticationRequest' smart constructor.
data DirectoryServiceAuthenticationRequest = DirectoryServiceAuthenticationRequest'
  { -- | The ID of the Active Directory to be used for authentication.
    directoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DirectoryServiceAuthenticationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'directoryServiceAuthenticationRequest_directoryId' - The ID of the Active Directory to be used for authentication.
newDirectoryServiceAuthenticationRequest ::
  DirectoryServiceAuthenticationRequest
newDirectoryServiceAuthenticationRequest =
  DirectoryServiceAuthenticationRequest'
    { directoryId =
        Prelude.Nothing
    }

-- | The ID of the Active Directory to be used for authentication.
directoryServiceAuthenticationRequest_directoryId :: Lens.Lens' DirectoryServiceAuthenticationRequest (Prelude.Maybe Prelude.Text)
directoryServiceAuthenticationRequest_directoryId = Lens.lens (\DirectoryServiceAuthenticationRequest' {directoryId} -> directoryId) (\s@DirectoryServiceAuthenticationRequest' {} a -> s {directoryId = a} :: DirectoryServiceAuthenticationRequest)

instance
  Prelude.Hashable
    DirectoryServiceAuthenticationRequest

instance
  Prelude.NFData
    DirectoryServiceAuthenticationRequest

instance
  Prelude.ToQuery
    DirectoryServiceAuthenticationRequest
  where
  toQuery DirectoryServiceAuthenticationRequest' {..} =
    Prelude.mconcat
      ["DirectoryId" Prelude.=: directoryId]
