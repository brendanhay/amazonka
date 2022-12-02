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
-- Module      : Amazonka.Wisdom.Types.AssistantSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AssistantSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.AssistantStatus
import Amazonka.Wisdom.Types.AssistantType
import Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration

-- | Summary information about the assistant.
--
-- /See:/ 'newAssistantSummary' smart constructor.
data AssistantSummary = AssistantSummary'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The KMS key used for encryption.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The description of the assistant.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Wisdom assistant.
    assistantArn :: Prelude.Text,
    -- | The identifier of the Wisdom assistant.
    assistantId :: Prelude.Text,
    -- | The name of the assistant.
    name :: Prelude.Text,
    -- | The status of the assistant.
    status :: AssistantStatus,
    -- | The type of the assistant.
    type' :: AssistantType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssistantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'assistantSummary_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'serverSideEncryptionConfiguration', 'assistantSummary_serverSideEncryptionConfiguration' - The KMS key used for encryption.
--
-- 'description', 'assistantSummary_description' - The description of the assistant.
--
-- 'assistantArn', 'assistantSummary_assistantArn' - The Amazon Resource Name (ARN) of the Wisdom assistant.
--
-- 'assistantId', 'assistantSummary_assistantId' - The identifier of the Wisdom assistant.
--
-- 'name', 'assistantSummary_name' - The name of the assistant.
--
-- 'status', 'assistantSummary_status' - The status of the assistant.
--
-- 'type'', 'assistantSummary_type' - The type of the assistant.
newAssistantSummary ::
  -- | 'assistantArn'
  Prelude.Text ->
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  AssistantStatus ->
  -- | 'type''
  AssistantType ->
  AssistantSummary
newAssistantSummary
  pAssistantArn_
  pAssistantId_
  pName_
  pStatus_
  pType_ =
    AssistantSummary'
      { tags = Prelude.Nothing,
        serverSideEncryptionConfiguration = Prelude.Nothing,
        description = Prelude.Nothing,
        assistantArn = pAssistantArn_,
        assistantId = pAssistantId_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | The tags used to organize, track, or control access for this resource.
assistantSummary_tags :: Lens.Lens' AssistantSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
assistantSummary_tags = Lens.lens (\AssistantSummary' {tags} -> tags) (\s@AssistantSummary' {} a -> s {tags = a} :: AssistantSummary) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key used for encryption.
assistantSummary_serverSideEncryptionConfiguration :: Lens.Lens' AssistantSummary (Prelude.Maybe ServerSideEncryptionConfiguration)
assistantSummary_serverSideEncryptionConfiguration = Lens.lens (\AssistantSummary' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@AssistantSummary' {} a -> s {serverSideEncryptionConfiguration = a} :: AssistantSummary)

-- | The description of the assistant.
assistantSummary_description :: Lens.Lens' AssistantSummary (Prelude.Maybe Prelude.Text)
assistantSummary_description = Lens.lens (\AssistantSummary' {description} -> description) (\s@AssistantSummary' {} a -> s {description = a} :: AssistantSummary)

-- | The Amazon Resource Name (ARN) of the Wisdom assistant.
assistantSummary_assistantArn :: Lens.Lens' AssistantSummary Prelude.Text
assistantSummary_assistantArn = Lens.lens (\AssistantSummary' {assistantArn} -> assistantArn) (\s@AssistantSummary' {} a -> s {assistantArn = a} :: AssistantSummary)

-- | The identifier of the Wisdom assistant.
assistantSummary_assistantId :: Lens.Lens' AssistantSummary Prelude.Text
assistantSummary_assistantId = Lens.lens (\AssistantSummary' {assistantId} -> assistantId) (\s@AssistantSummary' {} a -> s {assistantId = a} :: AssistantSummary)

-- | The name of the assistant.
assistantSummary_name :: Lens.Lens' AssistantSummary Prelude.Text
assistantSummary_name = Lens.lens (\AssistantSummary' {name} -> name) (\s@AssistantSummary' {} a -> s {name = a} :: AssistantSummary)

-- | The status of the assistant.
assistantSummary_status :: Lens.Lens' AssistantSummary AssistantStatus
assistantSummary_status = Lens.lens (\AssistantSummary' {status} -> status) (\s@AssistantSummary' {} a -> s {status = a} :: AssistantSummary)

-- | The type of the assistant.
assistantSummary_type :: Lens.Lens' AssistantSummary AssistantType
assistantSummary_type = Lens.lens (\AssistantSummary' {type'} -> type') (\s@AssistantSummary' {} a -> s {type' = a} :: AssistantSummary)

instance Data.FromJSON AssistantSummary where
  parseJSON =
    Data.withObject
      "AssistantSummary"
      ( \x ->
          AssistantSummary'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "serverSideEncryptionConfiguration")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "assistantArn")
            Prelude.<*> (x Data..: "assistantId")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AssistantSummary where
  hashWithSalt _salt AssistantSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` assistantArn
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssistantSummary where
  rnf AssistantSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf assistantArn
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
