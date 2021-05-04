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
-- Module      : Network.AWS.ResourceGroups.Types.PendingResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.PendingResource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that identifies a resource that is currently pending
-- addition to the group as a member. Adding a resource to a resource group
-- happens asynchronously as a background task and this one isn\'t
-- completed yet.
--
-- /See:/ 'newPendingResource' smart constructor.
data PendingResource = PendingResource'
  { -- | The Amazon resource name (ARN) of the resource that\'s in a pending
    -- state.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PendingResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'pendingResource_resourceArn' - The Amazon resource name (ARN) of the resource that\'s in a pending
-- state.
newPendingResource ::
  PendingResource
newPendingResource =
  PendingResource' {resourceArn = Prelude.Nothing}

-- | The Amazon resource name (ARN) of the resource that\'s in a pending
-- state.
pendingResource_resourceArn :: Lens.Lens' PendingResource (Prelude.Maybe Prelude.Text)
pendingResource_resourceArn = Lens.lens (\PendingResource' {resourceArn} -> resourceArn) (\s@PendingResource' {} a -> s {resourceArn = a} :: PendingResource)

instance Prelude.FromJSON PendingResource where
  parseJSON =
    Prelude.withObject
      "PendingResource"
      ( \x ->
          PendingResource'
            Prelude.<$> (x Prelude..:? "ResourceArn")
      )

instance Prelude.Hashable PendingResource

instance Prelude.NFData PendingResource
