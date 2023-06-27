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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the basic information about the code generation job.
--
-- /See:/ 'newCodegenJobSummary' smart constructor.
data CodegenJobSummary = CodegenJobSummary'
  { -- | The time that the code generation job summary was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The time that the code generation job summary was modified.
    modifiedAt :: Prelude.Maybe Data.ISO8601,
    -- | The unique ID of the Amplify app associated with the code generation
    -- job.
    appId :: Prelude.Text,
    -- | The name of the backend environment associated with the code generation
    -- job.
    environmentName :: Prelude.Text,
    -- | The unique ID for the code generation job summary.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'codegenJobSummary_createdAt' - The time that the code generation job summary was created.
--
-- 'modifiedAt', 'codegenJobSummary_modifiedAt' - The time that the code generation job summary was modified.
--
-- 'appId', 'codegenJobSummary_appId' - The unique ID of the Amplify app associated with the code generation
-- job.
--
-- 'environmentName', 'codegenJobSummary_environmentName' - The name of the backend environment associated with the code generation
-- job.
--
-- 'id', 'codegenJobSummary_id' - The unique ID for the code generation job summary.
newCodegenJobSummary ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  CodegenJobSummary
newCodegenJobSummary pAppId_ pEnvironmentName_ pId_ =
  CodegenJobSummary'
    { createdAt = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The time that the code generation job summary was created.
codegenJobSummary_createdAt :: Lens.Lens' CodegenJobSummary (Prelude.Maybe Prelude.UTCTime)
codegenJobSummary_createdAt = Lens.lens (\CodegenJobSummary' {createdAt} -> createdAt) (\s@CodegenJobSummary' {} a -> s {createdAt = a} :: CodegenJobSummary) Prelude.. Lens.mapping Data._Time

-- | The time that the code generation job summary was modified.
codegenJobSummary_modifiedAt :: Lens.Lens' CodegenJobSummary (Prelude.Maybe Prelude.UTCTime)
codegenJobSummary_modifiedAt = Lens.lens (\CodegenJobSummary' {modifiedAt} -> modifiedAt) (\s@CodegenJobSummary' {} a -> s {modifiedAt = a} :: CodegenJobSummary) Prelude.. Lens.mapping Data._Time

-- | The unique ID of the Amplify app associated with the code generation
-- job.
codegenJobSummary_appId :: Lens.Lens' CodegenJobSummary Prelude.Text
codegenJobSummary_appId = Lens.lens (\CodegenJobSummary' {appId} -> appId) (\s@CodegenJobSummary' {} a -> s {appId = a} :: CodegenJobSummary)

-- | The name of the backend environment associated with the code generation
-- job.
codegenJobSummary_environmentName :: Lens.Lens' CodegenJobSummary Prelude.Text
codegenJobSummary_environmentName = Lens.lens (\CodegenJobSummary' {environmentName} -> environmentName) (\s@CodegenJobSummary' {} a -> s {environmentName = a} :: CodegenJobSummary)

-- | The unique ID for the code generation job summary.
codegenJobSummary_id :: Lens.Lens' CodegenJobSummary Prelude.Text
codegenJobSummary_id = Lens.lens (\CodegenJobSummary' {id} -> id) (\s@CodegenJobSummary' {} a -> s {id = a} :: CodegenJobSummary)

instance Data.FromJSON CodegenJobSummary where
  parseJSON =
    Data.withObject
      "CodegenJobSummary"
      ( \x ->
          CodegenJobSummary'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "modifiedAt")
            Prelude.<*> (x Data..: "appId")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable CodegenJobSummary where
  hashWithSalt _salt CodegenJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData CodegenJobSummary where
  rnf CodegenJobSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
