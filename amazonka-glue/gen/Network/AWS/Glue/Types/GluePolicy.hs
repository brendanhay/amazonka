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
-- Module      : Network.AWS.Glue.Types.GluePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GluePolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure for returning a resource policy.
--
-- /See:/ 'newGluePolicy' smart constructor.
data GluePolicy = GluePolicy'
  { -- | Contains the requested policy document, in JSON format.
    policyInJson :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the policy was last updated.
    updateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time at which the policy was created.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | Contains the hash value associated with this policy.
    policyHash :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { policyInJson = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      policyHash = Prelude.Nothing
    }

-- | Contains the requested policy document, in JSON format.
gluePolicy_policyInJson :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.Text)
gluePolicy_policyInJson = Lens.lens (\GluePolicy' {policyInJson} -> policyInJson) (\s@GluePolicy' {} a -> s {policyInJson = a} :: GluePolicy)

-- | The date and time at which the policy was last updated.
gluePolicy_updateTime :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.UTCTime)
gluePolicy_updateTime = Lens.lens (\GluePolicy' {updateTime} -> updateTime) (\s@GluePolicy' {} a -> s {updateTime = a} :: GluePolicy) Prelude.. Lens.mapping Prelude._Time

-- | The date and time at which the policy was created.
gluePolicy_createTime :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.UTCTime)
gluePolicy_createTime = Lens.lens (\GluePolicy' {createTime} -> createTime) (\s@GluePolicy' {} a -> s {createTime = a} :: GluePolicy) Prelude.. Lens.mapping Prelude._Time

-- | Contains the hash value associated with this policy.
gluePolicy_policyHash :: Lens.Lens' GluePolicy (Prelude.Maybe Prelude.Text)
gluePolicy_policyHash = Lens.lens (\GluePolicy' {policyHash} -> policyHash) (\s@GluePolicy' {} a -> s {policyHash = a} :: GluePolicy)

instance Prelude.FromJSON GluePolicy where
  parseJSON =
    Prelude.withObject
      "GluePolicy"
      ( \x ->
          GluePolicy'
            Prelude.<$> (x Prelude..:? "PolicyInJson")
            Prelude.<*> (x Prelude..:? "UpdateTime")
            Prelude.<*> (x Prelude..:? "CreateTime")
            Prelude.<*> (x Prelude..:? "PolicyHash")
      )

instance Prelude.Hashable GluePolicy

instance Prelude.NFData GluePolicy
