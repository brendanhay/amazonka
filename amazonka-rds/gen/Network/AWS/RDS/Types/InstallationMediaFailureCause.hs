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
-- Module      : Network.AWS.RDS.Types.InstallationMediaFailureCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.InstallationMediaFailureCause where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the cause of an installation media failure. Installation media
-- is used for a DB engine that requires an on-premises customer provided
-- license, such as Microsoft SQL Server.
--
-- /See:/ 'newInstallationMediaFailureCause' smart constructor.
data InstallationMediaFailureCause = InstallationMediaFailureCause'
  { -- | The reason that an installation media import failed.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstallationMediaFailureCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'installationMediaFailureCause_message' - The reason that an installation media import failed.
newInstallationMediaFailureCause ::
  InstallationMediaFailureCause
newInstallationMediaFailureCause =
  InstallationMediaFailureCause'
    { message =
        Prelude.Nothing
    }

-- | The reason that an installation media import failed.
installationMediaFailureCause_message :: Lens.Lens' InstallationMediaFailureCause (Prelude.Maybe Prelude.Text)
installationMediaFailureCause_message = Lens.lens (\InstallationMediaFailureCause' {message} -> message) (\s@InstallationMediaFailureCause' {} a -> s {message = a} :: InstallationMediaFailureCause)

instance
  Prelude.FromXML
    InstallationMediaFailureCause
  where
  parseXML x =
    InstallationMediaFailureCause'
      Prelude.<$> (x Prelude..@? "Message")

instance
  Prelude.Hashable
    InstallationMediaFailureCause

instance Prelude.NFData InstallationMediaFailureCause
