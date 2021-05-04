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
-- Module      : Network.AWS.IoT.Types.PolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a policy version.
--
-- /See:/ 'newPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { -- | The date and time the policy was created.
    createDate :: Prelude.Maybe Prelude.POSIX,
    -- | The policy version ID.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'policyVersion_createDate' - The date and time the policy was created.
--
-- 'versionId', 'policyVersion_versionId' - The policy version ID.
--
-- 'isDefaultVersion', 'policyVersion_isDefaultVersion' - Specifies whether the policy version is the default.
newPolicyVersion ::
  PolicyVersion
newPolicyVersion =
  PolicyVersion'
    { createDate = Prelude.Nothing,
      versionId = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing
    }

-- | The date and time the policy was created.
policyVersion_createDate :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.UTCTime)
policyVersion_createDate = Lens.lens (\PolicyVersion' {createDate} -> createDate) (\s@PolicyVersion' {} a -> s {createDate = a} :: PolicyVersion) Prelude.. Lens.mapping Prelude._Time

-- | The policy version ID.
policyVersion_versionId :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Text)
policyVersion_versionId = Lens.lens (\PolicyVersion' {versionId} -> versionId) (\s@PolicyVersion' {} a -> s {versionId = a} :: PolicyVersion)

-- | Specifies whether the policy version is the default.
policyVersion_isDefaultVersion :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Bool)
policyVersion_isDefaultVersion = Lens.lens (\PolicyVersion' {isDefaultVersion} -> isDefaultVersion) (\s@PolicyVersion' {} a -> s {isDefaultVersion = a} :: PolicyVersion)

instance Prelude.FromJSON PolicyVersion where
  parseJSON =
    Prelude.withObject
      "PolicyVersion"
      ( \x ->
          PolicyVersion'
            Prelude.<$> (x Prelude..:? "createDate")
            Prelude.<*> (x Prelude..:? "versionId")
            Prelude.<*> (x Prelude..:? "isDefaultVersion")
      )

instance Prelude.Hashable PolicyVersion

instance Prelude.NFData PolicyVersion
