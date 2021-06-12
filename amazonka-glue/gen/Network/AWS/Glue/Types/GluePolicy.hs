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
-- Module      : Network.AWS.Glue.Types.GluePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GluePolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure for returning a resource policy.
--
-- /See:/ 'newGluePolicy' smart constructor.
data GluePolicy = GluePolicy'
  { -- | Contains the requested policy document, in JSON format.
    policyInJson :: Core.Maybe Core.Text,
    -- | The date and time at which the policy was last updated.
    updateTime :: Core.Maybe Core.POSIX,
    -- | The date and time at which the policy was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | Contains the hash value associated with this policy.
    policyHash :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GluePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyInJson', 'gluePolicy_policyInJson' - Contains the requested policy document, in JSON format.
--
-- 'updateTime', 'gluePolicy_updateTime' - The date and time at which the policy was last updated.
--
-- 'createTime', 'gluePolicy_createTime' - The date and time at which the policy was created.
--
-- 'policyHash', 'gluePolicy_policyHash' - Contains the hash value associated with this policy.
newGluePolicy ::
  GluePolicy
newGluePolicy =
  GluePolicy'
    { policyInJson = Core.Nothing,
      updateTime = Core.Nothing,
      createTime = Core.Nothing,
      policyHash = Core.Nothing
    }

-- | Contains the requested policy document, in JSON format.
gluePolicy_policyInJson :: Lens.Lens' GluePolicy (Core.Maybe Core.Text)
gluePolicy_policyInJson = Lens.lens (\GluePolicy' {policyInJson} -> policyInJson) (\s@GluePolicy' {} a -> s {policyInJson = a} :: GluePolicy)

-- | The date and time at which the policy was last updated.
gluePolicy_updateTime :: Lens.Lens' GluePolicy (Core.Maybe Core.UTCTime)
gluePolicy_updateTime = Lens.lens (\GluePolicy' {updateTime} -> updateTime) (\s@GluePolicy' {} a -> s {updateTime = a} :: GluePolicy) Core.. Lens.mapping Core._Time

-- | The date and time at which the policy was created.
gluePolicy_createTime :: Lens.Lens' GluePolicy (Core.Maybe Core.UTCTime)
gluePolicy_createTime = Lens.lens (\GluePolicy' {createTime} -> createTime) (\s@GluePolicy' {} a -> s {createTime = a} :: GluePolicy) Core.. Lens.mapping Core._Time

-- | Contains the hash value associated with this policy.
gluePolicy_policyHash :: Lens.Lens' GluePolicy (Core.Maybe Core.Text)
gluePolicy_policyHash = Lens.lens (\GluePolicy' {policyHash} -> policyHash) (\s@GluePolicy' {} a -> s {policyHash = a} :: GluePolicy)

instance Core.FromJSON GluePolicy where
  parseJSON =
    Core.withObject
      "GluePolicy"
      ( \x ->
          GluePolicy'
            Core.<$> (x Core..:? "PolicyInJson")
            Core.<*> (x Core..:? "UpdateTime")
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "PolicyHash")
      )

instance Core.Hashable GluePolicy

instance Core.NFData GluePolicy
