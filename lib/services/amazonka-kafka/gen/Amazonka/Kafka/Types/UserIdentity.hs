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
-- Module      : Amazonka.Kafka.Types.UserIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.UserIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.UserIdentityType
import qualified Amazonka.Prelude as Prelude

-- | Description of the requester that calls the API operation.
--
-- /See:/ 'newUserIdentity' smart constructor.
data UserIdentity = UserIdentity'
  { -- | A unique identifier for the requester that calls the API operation.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The identity type of the requester that calls the API operation.
    type' :: Prelude.Maybe UserIdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'userIdentity_principalId' - A unique identifier for the requester that calls the API operation.
--
-- 'type'', 'userIdentity_type' - The identity type of the requester that calls the API operation.
newUserIdentity ::
  UserIdentity
newUserIdentity =
  UserIdentity'
    { principalId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A unique identifier for the requester that calls the API operation.
userIdentity_principalId :: Lens.Lens' UserIdentity (Prelude.Maybe Prelude.Text)
userIdentity_principalId = Lens.lens (\UserIdentity' {principalId} -> principalId) (\s@UserIdentity' {} a -> s {principalId = a} :: UserIdentity)

-- | The identity type of the requester that calls the API operation.
userIdentity_type :: Lens.Lens' UserIdentity (Prelude.Maybe UserIdentityType)
userIdentity_type = Lens.lens (\UserIdentity' {type'} -> type') (\s@UserIdentity' {} a -> s {type' = a} :: UserIdentity)

instance Data.FromJSON UserIdentity where
  parseJSON =
    Data.withObject
      "UserIdentity"
      ( \x ->
          UserIdentity'
            Prelude.<$> (x Data..:? "principalId")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable UserIdentity where
  hashWithSalt _salt UserIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UserIdentity where
  rnf UserIdentity' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf type'
