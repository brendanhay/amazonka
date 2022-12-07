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
-- Module      : Amazonka.CloudDirectory.Types.PolicyToPath
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.PolicyToPath where

import Amazonka.CloudDirectory.Types.PolicyAttachment
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used when a regular object exists in a Directory and you want to find
-- all of the policies that are associated with that object and the parent
-- to that object.
--
-- /See:/ 'newPolicyToPath' smart constructor.
data PolicyToPath = PolicyToPath'
  { -- | The path that is referenced from the root.
    path :: Prelude.Maybe Prelude.Text,
    -- | List of policy objects.
    policies :: Prelude.Maybe [PolicyAttachment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyToPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'policyToPath_path' - The path that is referenced from the root.
--
-- 'policies', 'policyToPath_policies' - List of policy objects.
newPolicyToPath ::
  PolicyToPath
newPolicyToPath =
  PolicyToPath'
    { path = Prelude.Nothing,
      policies = Prelude.Nothing
    }

-- | The path that is referenced from the root.
policyToPath_path :: Lens.Lens' PolicyToPath (Prelude.Maybe Prelude.Text)
policyToPath_path = Lens.lens (\PolicyToPath' {path} -> path) (\s@PolicyToPath' {} a -> s {path = a} :: PolicyToPath)

-- | List of policy objects.
policyToPath_policies :: Lens.Lens' PolicyToPath (Prelude.Maybe [PolicyAttachment])
policyToPath_policies = Lens.lens (\PolicyToPath' {policies} -> policies) (\s@PolicyToPath' {} a -> s {policies = a} :: PolicyToPath) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PolicyToPath where
  parseJSON =
    Data.withObject
      "PolicyToPath"
      ( \x ->
          PolicyToPath'
            Prelude.<$> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "Policies" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PolicyToPath where
  hashWithSalt _salt PolicyToPath' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` policies

instance Prelude.NFData PolicyToPath where
  rnf PolicyToPath' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf policies
