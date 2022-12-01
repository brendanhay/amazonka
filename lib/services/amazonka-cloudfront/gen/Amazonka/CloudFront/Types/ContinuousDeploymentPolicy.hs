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
-- Module      : Amazonka.CloudFront.Types.ContinuousDeploymentPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContinuousDeploymentPolicy where

import Amazonka.CloudFront.Types.ContinuousDeploymentPolicyConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A continuous deployment policy.
--
-- /See:/ 'newContinuousDeploymentPolicy' smart constructor.
data ContinuousDeploymentPolicy = ContinuousDeploymentPolicy'
  { -- | The identifier of the continuous deployment policy.
    id :: Prelude.Text,
    -- | The date and time the continuous deployment policy was last modified.
    lastModifiedTime :: Core.ISO8601,
    continuousDeploymentPolicyConfig :: ContinuousDeploymentPolicyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousDeploymentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'continuousDeploymentPolicy_id' - The identifier of the continuous deployment policy.
--
-- 'lastModifiedTime', 'continuousDeploymentPolicy_lastModifiedTime' - The date and time the continuous deployment policy was last modified.
--
-- 'continuousDeploymentPolicyConfig', 'continuousDeploymentPolicy_continuousDeploymentPolicyConfig' - Undocumented member.
newContinuousDeploymentPolicy ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'continuousDeploymentPolicyConfig'
  ContinuousDeploymentPolicyConfig ->
  ContinuousDeploymentPolicy
newContinuousDeploymentPolicy
  pId_
  pLastModifiedTime_
  pContinuousDeploymentPolicyConfig_ =
    ContinuousDeploymentPolicy'
      { id = pId_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        continuousDeploymentPolicyConfig =
          pContinuousDeploymentPolicyConfig_
      }

-- | The identifier of the continuous deployment policy.
continuousDeploymentPolicy_id :: Lens.Lens' ContinuousDeploymentPolicy Prelude.Text
continuousDeploymentPolicy_id = Lens.lens (\ContinuousDeploymentPolicy' {id} -> id) (\s@ContinuousDeploymentPolicy' {} a -> s {id = a} :: ContinuousDeploymentPolicy)

-- | The date and time the continuous deployment policy was last modified.
continuousDeploymentPolicy_lastModifiedTime :: Lens.Lens' ContinuousDeploymentPolicy Prelude.UTCTime
continuousDeploymentPolicy_lastModifiedTime = Lens.lens (\ContinuousDeploymentPolicy' {lastModifiedTime} -> lastModifiedTime) (\s@ContinuousDeploymentPolicy' {} a -> s {lastModifiedTime = a} :: ContinuousDeploymentPolicy) Prelude.. Core._Time

-- | Undocumented member.
continuousDeploymentPolicy_continuousDeploymentPolicyConfig :: Lens.Lens' ContinuousDeploymentPolicy ContinuousDeploymentPolicyConfig
continuousDeploymentPolicy_continuousDeploymentPolicyConfig = Lens.lens (\ContinuousDeploymentPolicy' {continuousDeploymentPolicyConfig} -> continuousDeploymentPolicyConfig) (\s@ContinuousDeploymentPolicy' {} a -> s {continuousDeploymentPolicyConfig = a} :: ContinuousDeploymentPolicy)

instance Core.FromXML ContinuousDeploymentPolicy where
  parseXML x =
    ContinuousDeploymentPolicy'
      Prelude.<$> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "LastModifiedTime")
      Prelude.<*> (x Core..@ "ContinuousDeploymentPolicyConfig")

instance Prelude.Hashable ContinuousDeploymentPolicy where
  hashWithSalt _salt ContinuousDeploymentPolicy' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` continuousDeploymentPolicyConfig

instance Prelude.NFData ContinuousDeploymentPolicy where
  rnf ContinuousDeploymentPolicy' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf continuousDeploymentPolicyConfig
