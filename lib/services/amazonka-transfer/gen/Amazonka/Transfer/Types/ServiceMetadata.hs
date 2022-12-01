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
-- Module      : Amazonka.Transfer.Types.ServiceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ServiceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.UserDetails

-- | A container object for the session details that are associated with a
-- workflow.
--
-- /See:/ 'newServiceMetadata' smart constructor.
data ServiceMetadata = ServiceMetadata'
  { -- | The Server ID (@ServerId@), Session ID (@SessionId@) and user
    -- (@UserName@) make up the @UserDetails@.
    userDetails :: UserDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userDetails', 'serviceMetadata_userDetails' - The Server ID (@ServerId@), Session ID (@SessionId@) and user
-- (@UserName@) make up the @UserDetails@.
newServiceMetadata ::
  -- | 'userDetails'
  UserDetails ->
  ServiceMetadata
newServiceMetadata pUserDetails_ =
  ServiceMetadata' {userDetails = pUserDetails_}

-- | The Server ID (@ServerId@), Session ID (@SessionId@) and user
-- (@UserName@) make up the @UserDetails@.
serviceMetadata_userDetails :: Lens.Lens' ServiceMetadata UserDetails
serviceMetadata_userDetails = Lens.lens (\ServiceMetadata' {userDetails} -> userDetails) (\s@ServiceMetadata' {} a -> s {userDetails = a} :: ServiceMetadata)

instance Core.FromJSON ServiceMetadata where
  parseJSON =
    Core.withObject
      "ServiceMetadata"
      ( \x ->
          ServiceMetadata'
            Prelude.<$> (x Core..: "UserDetails")
      )

instance Prelude.Hashable ServiceMetadata where
  hashWithSalt _salt ServiceMetadata' {..} =
    _salt `Prelude.hashWithSalt` userDetails

instance Prelude.NFData ServiceMetadata where
  rnf ServiceMetadata' {..} = Prelude.rnf userDetails
