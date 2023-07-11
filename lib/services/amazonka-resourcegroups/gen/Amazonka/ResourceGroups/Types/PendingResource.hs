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
-- Module      : Amazonka.ResourceGroups.Types.PendingResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.PendingResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON PendingResource where
  parseJSON =
    Data.withObject
      "PendingResource"
      ( \x ->
          PendingResource'
            Prelude.<$> (x Data..:? "ResourceArn")
      )

instance Prelude.Hashable PendingResource where
  hashWithSalt _salt PendingResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData PendingResource where
  rnf PendingResource' {..} = Prelude.rnf resourceArn
