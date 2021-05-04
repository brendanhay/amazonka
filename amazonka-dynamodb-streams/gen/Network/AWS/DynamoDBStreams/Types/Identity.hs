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
-- Module      : Network.AWS.DynamoDBStreams.Types.Identity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Identity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Identity where
  parseJSON =
    Prelude.withObject
      "Identity"
      ( \x ->
          Identity'
            Prelude.<$> (x Prelude..:? "PrincipalId")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Identity

instance Prelude.NFData Identity
