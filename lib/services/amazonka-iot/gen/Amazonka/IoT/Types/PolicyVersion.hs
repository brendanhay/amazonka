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
-- Module      : Amazonka.IoT.Types.PolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PolicyVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a policy version.
--
-- /See:/ 'newPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { -- | The policy version ID.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the policy was created.
    createDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'policyVersion_versionId' - The policy version ID.
--
-- 'createDate', 'policyVersion_createDate' - The date and time the policy was created.
--
-- 'isDefaultVersion', 'policyVersion_isDefaultVersion' - Specifies whether the policy version is the default.
newPolicyVersion ::
  PolicyVersion
newPolicyVersion =
  PolicyVersion'
    { versionId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing
    }

-- | The policy version ID.
policyVersion_versionId :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Text)
policyVersion_versionId = Lens.lens (\PolicyVersion' {versionId} -> versionId) (\s@PolicyVersion' {} a -> s {versionId = a} :: PolicyVersion)

-- | The date and time the policy was created.
policyVersion_createDate :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.UTCTime)
policyVersion_createDate = Lens.lens (\PolicyVersion' {createDate} -> createDate) (\s@PolicyVersion' {} a -> s {createDate = a} :: PolicyVersion) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the policy version is the default.
policyVersion_isDefaultVersion :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Bool)
policyVersion_isDefaultVersion = Lens.lens (\PolicyVersion' {isDefaultVersion} -> isDefaultVersion) (\s@PolicyVersion' {} a -> s {isDefaultVersion = a} :: PolicyVersion)

instance Core.FromJSON PolicyVersion where
  parseJSON =
    Core.withObject
      "PolicyVersion"
      ( \x ->
          PolicyVersion'
            Prelude.<$> (x Core..:? "versionId")
            Prelude.<*> (x Core..:? "createDate")
            Prelude.<*> (x Core..:? "isDefaultVersion")
      )

instance Prelude.Hashable PolicyVersion where
  hashWithSalt salt' PolicyVersion' {..} =
    salt' `Prelude.hashWithSalt` isDefaultVersion
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData PolicyVersion where
  rnf PolicyVersion' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf createDate
