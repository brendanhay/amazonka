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
-- Module      : Amazonka.Rekognition.Types.ProjectPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProjectPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a project policy in the response from ListProjectPolicies.
--
-- /See:/ 'newProjectPolicy' smart constructor.
data ProjectPolicy = ProjectPolicy'
  { -- | The name of the project policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The Unix datetime for when the project policy was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The Unix datetime for the creation of the project policy.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The revision ID of the project policy.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The JSON document for the project policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project to which the project
    -- policy is attached.
    projectArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'projectPolicy_policyName' - The name of the project policy.
--
-- 'lastUpdatedTimestamp', 'projectPolicy_lastUpdatedTimestamp' - The Unix datetime for when the project policy was last updated.
--
-- 'creationTimestamp', 'projectPolicy_creationTimestamp' - The Unix datetime for the creation of the project policy.
--
-- 'policyRevisionId', 'projectPolicy_policyRevisionId' - The revision ID of the project policy.
--
-- 'policyDocument', 'projectPolicy_policyDocument' - The JSON document for the project policy.
--
-- 'projectArn', 'projectPolicy_projectArn' - The Amazon Resource Name (ARN) of the project to which the project
-- policy is attached.
newProjectPolicy ::
  ProjectPolicy
newProjectPolicy =
  ProjectPolicy'
    { policyName = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      policyRevisionId = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      projectArn = Prelude.Nothing
    }

-- | The name of the project policy.
projectPolicy_policyName :: Lens.Lens' ProjectPolicy (Prelude.Maybe Prelude.Text)
projectPolicy_policyName = Lens.lens (\ProjectPolicy' {policyName} -> policyName) (\s@ProjectPolicy' {} a -> s {policyName = a} :: ProjectPolicy)

-- | The Unix datetime for when the project policy was last updated.
projectPolicy_lastUpdatedTimestamp :: Lens.Lens' ProjectPolicy (Prelude.Maybe Prelude.UTCTime)
projectPolicy_lastUpdatedTimestamp = Lens.lens (\ProjectPolicy' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ProjectPolicy' {} a -> s {lastUpdatedTimestamp = a} :: ProjectPolicy) Prelude.. Lens.mapping Data._Time

-- | The Unix datetime for the creation of the project policy.
projectPolicy_creationTimestamp :: Lens.Lens' ProjectPolicy (Prelude.Maybe Prelude.UTCTime)
projectPolicy_creationTimestamp = Lens.lens (\ProjectPolicy' {creationTimestamp} -> creationTimestamp) (\s@ProjectPolicy' {} a -> s {creationTimestamp = a} :: ProjectPolicy) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the project policy.
projectPolicy_policyRevisionId :: Lens.Lens' ProjectPolicy (Prelude.Maybe Prelude.Text)
projectPolicy_policyRevisionId = Lens.lens (\ProjectPolicy' {policyRevisionId} -> policyRevisionId) (\s@ProjectPolicy' {} a -> s {policyRevisionId = a} :: ProjectPolicy)

-- | The JSON document for the project policy.
projectPolicy_policyDocument :: Lens.Lens' ProjectPolicy (Prelude.Maybe Prelude.Text)
projectPolicy_policyDocument = Lens.lens (\ProjectPolicy' {policyDocument} -> policyDocument) (\s@ProjectPolicy' {} a -> s {policyDocument = a} :: ProjectPolicy)

-- | The Amazon Resource Name (ARN) of the project to which the project
-- policy is attached.
projectPolicy_projectArn :: Lens.Lens' ProjectPolicy (Prelude.Maybe Prelude.Text)
projectPolicy_projectArn = Lens.lens (\ProjectPolicy' {projectArn} -> projectArn) (\s@ProjectPolicy' {} a -> s {projectArn = a} :: ProjectPolicy)

instance Data.FromJSON ProjectPolicy where
  parseJSON =
    Data.withObject
      "ProjectPolicy"
      ( \x ->
          ProjectPolicy'
            Prelude.<$> (x Data..:? "PolicyName")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "PolicyRevisionId")
            Prelude.<*> (x Data..:? "PolicyDocument")
            Prelude.<*> (x Data..:? "ProjectArn")
      )

instance Prelude.Hashable ProjectPolicy where
  hashWithSalt _salt ProjectPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData ProjectPolicy where
  rnf ProjectPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf projectArn
