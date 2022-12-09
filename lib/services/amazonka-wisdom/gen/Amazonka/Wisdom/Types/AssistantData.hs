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
-- Module      : Amazonka.Wisdom.Types.AssistantData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AssistantData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.AssistantStatus
import Amazonka.Wisdom.Types.AssistantType
import Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration

-- | The assistant data.
--
-- /See:/ 'newAssistantData' smart constructor.
data AssistantData = AssistantData'
  { -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The KMS key used for encryption.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the Wisdom assistant.
    assistantArn :: Prelude.Text,
    -- | The identifier of the Wisdom assistant.
    assistantId :: Prelude.Text,
    -- | The name.
    name :: Prelude.Text,
    -- | The status of the assistant.
    status :: AssistantStatus,
    -- | The type of assistant.
    type' :: AssistantType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssistantData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'assistantData_description' - The description.
--
-- 'serverSideEncryptionConfiguration', 'assistantData_serverSideEncryptionConfiguration' - The KMS key used for encryption.
--
-- 'tags', 'assistantData_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'assistantArn', 'assistantData_assistantArn' - The Amazon Resource Name (ARN) of the Wisdom assistant.
--
-- 'assistantId', 'assistantData_assistantId' - The identifier of the Wisdom assistant.
--
-- 'name', 'assistantData_name' - The name.
--
-- 'status', 'assistantData_status' - The status of the assistant.
--
-- 'type'', 'assistantData_type' - The type of assistant.
newAssistantData ::
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
  AssistantData
newAssistantData
  pAssistantArn_
  pAssistantId_
  pName_
  pStatus_
  pType_ =
    AssistantData'
      { description = Prelude.Nothing,
        serverSideEncryptionConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        assistantArn = pAssistantArn_,
        assistantId = pAssistantId_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | The description.
assistantData_description :: Lens.Lens' AssistantData (Prelude.Maybe Prelude.Text)
assistantData_description = Lens.lens (\AssistantData' {description} -> description) (\s@AssistantData' {} a -> s {description = a} :: AssistantData)

-- | The KMS key used for encryption.
assistantData_serverSideEncryptionConfiguration :: Lens.Lens' AssistantData (Prelude.Maybe ServerSideEncryptionConfiguration)
assistantData_serverSideEncryptionConfiguration = Lens.lens (\AssistantData' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@AssistantData' {} a -> s {serverSideEncryptionConfiguration = a} :: AssistantData)

-- | The tags used to organize, track, or control access for this resource.
assistantData_tags :: Lens.Lens' AssistantData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
assistantData_tags = Lens.lens (\AssistantData' {tags} -> tags) (\s@AssistantData' {} a -> s {tags = a} :: AssistantData) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Wisdom assistant.
assistantData_assistantArn :: Lens.Lens' AssistantData Prelude.Text
assistantData_assistantArn = Lens.lens (\AssistantData' {assistantArn} -> assistantArn) (\s@AssistantData' {} a -> s {assistantArn = a} :: AssistantData)

-- | The identifier of the Wisdom assistant.
assistantData_assistantId :: Lens.Lens' AssistantData Prelude.Text
assistantData_assistantId = Lens.lens (\AssistantData' {assistantId} -> assistantId) (\s@AssistantData' {} a -> s {assistantId = a} :: AssistantData)

-- | The name.
assistantData_name :: Lens.Lens' AssistantData Prelude.Text
assistantData_name = Lens.lens (\AssistantData' {name} -> name) (\s@AssistantData' {} a -> s {name = a} :: AssistantData)

-- | The status of the assistant.
assistantData_status :: Lens.Lens' AssistantData AssistantStatus
assistantData_status = Lens.lens (\AssistantData' {status} -> status) (\s@AssistantData' {} a -> s {status = a} :: AssistantData)

-- | The type of assistant.
assistantData_type :: Lens.Lens' AssistantData AssistantType
assistantData_type = Lens.lens (\AssistantData' {type'} -> type') (\s@AssistantData' {} a -> s {type' = a} :: AssistantData)

instance Data.FromJSON AssistantData where
  parseJSON =
    Data.withObject
      "AssistantData"
      ( \x ->
          AssistantData'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "serverSideEncryptionConfiguration")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "assistantArn")
            Prelude.<*> (x Data..: "assistantId")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AssistantData where
  hashWithSalt _salt AssistantData' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assistantArn
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssistantData where
  rnf AssistantData' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf assistantArn
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
