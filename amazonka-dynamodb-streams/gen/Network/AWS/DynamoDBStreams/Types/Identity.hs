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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about the type of identity that made the request.
--
-- /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | A unique identifier for the entity that made the call. For Time To Live,
    -- the principalId is \"dynamodb.amazonaws.com\".
    principalId :: Core.Maybe Core.Text,
    -- | The type of the identity. For Time To Live, the type is \"Service\".
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { principalId = Core.Nothing,
      type' = Core.Nothing
    }

-- | A unique identifier for the entity that made the call. For Time To Live,
-- the principalId is \"dynamodb.amazonaws.com\".
identity_principalId :: Lens.Lens' Identity (Core.Maybe Core.Text)
identity_principalId = Lens.lens (\Identity' {principalId} -> principalId) (\s@Identity' {} a -> s {principalId = a} :: Identity)

-- | The type of the identity. For Time To Live, the type is \"Service\".
identity_type :: Lens.Lens' Identity (Core.Maybe Core.Text)
identity_type = Lens.lens (\Identity' {type'} -> type') (\s@Identity' {} a -> s {type' = a} :: Identity)

instance Core.FromJSON Identity where
  parseJSON =
    Core.withObject
      "Identity"
      ( \x ->
          Identity'
            Core.<$> (x Core..:? "PrincipalId")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Identity

instance Core.NFData Identity
