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
-- Module      : Amazonka.DynamoDBStreams.Types.Identity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.Identity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDBStreams.Internal
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the type of identity that made the request.
--
-- /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | A unique identifier for the entity that made the call. For Time To Live,
    -- the principalId is \"dynamodb.amazonaws.com\".
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The type of the identity. For Time To Live, the type is \"Service\".
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Identity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'identity_principalId' - A unique identifier for the entity that made the call. For Time To Live,
-- the principalId is \"dynamodb.amazonaws.com\".
--
-- 'type'', 'identity_type' - The type of the identity. For Time To Live, the type is \"Service\".
newIdentity ::
  Identity
newIdentity =
  Identity'
    { principalId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A unique identifier for the entity that made the call. For Time To Live,
-- the principalId is \"dynamodb.amazonaws.com\".
identity_principalId :: Lens.Lens' Identity (Prelude.Maybe Prelude.Text)
identity_principalId = Lens.lens (\Identity' {principalId} -> principalId) (\s@Identity' {} a -> s {principalId = a} :: Identity)

-- | The type of the identity. For Time To Live, the type is \"Service\".
identity_type :: Lens.Lens' Identity (Prelude.Maybe Prelude.Text)
identity_type = Lens.lens (\Identity' {type'} -> type') (\s@Identity' {} a -> s {type' = a} :: Identity)

instance Data.FromJSON Identity where
  parseJSON =
    Data.withObject
      "Identity"
      ( \x ->
          Identity'
            Prelude.<$> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Identity where
  hashWithSalt _salt Identity' {..} =
    _salt
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Identity where
  rnf Identity' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf type'
