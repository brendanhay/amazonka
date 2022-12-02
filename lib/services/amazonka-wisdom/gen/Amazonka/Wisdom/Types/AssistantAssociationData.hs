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
-- Module      : Amazonka.Wisdom.Types.AssistantAssociationData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AssistantAssociationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.AssistantAssociationOutputData
import Amazonka.Wisdom.Types.AssociationType

-- | Information about the assistant association.
--
-- /See:/ 'newAssistantAssociationData' smart constructor.
data AssistantAssociationData = AssistantAssociationData'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the Wisdom assistant.
    assistantArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the assistant association.
    assistantAssociationArn :: Prelude.Text,
    -- | The identifier of the assistant association.
    assistantAssociationId :: Prelude.Text,
    -- | The identifier of the Wisdom assistant.
    assistantId :: Prelude.Text,
    -- | A union type that currently has a single argument, the knowledge base
    -- ID.
    associationData :: AssistantAssociationOutputData,
    -- | The type of association.
    associationType :: AssociationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssistantAssociationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'assistantAssociationData_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'assistantArn', 'assistantAssociationData_assistantArn' - The Amazon Resource Name (ARN) of the Wisdom assistant.
--
-- 'assistantAssociationArn', 'assistantAssociationData_assistantAssociationArn' - The Amazon Resource Name (ARN) of the assistant association.
--
-- 'assistantAssociationId', 'assistantAssociationData_assistantAssociationId' - The identifier of the assistant association.
--
-- 'assistantId', 'assistantAssociationData_assistantId' - The identifier of the Wisdom assistant.
--
-- 'associationData', 'assistantAssociationData_associationData' - A union type that currently has a single argument, the knowledge base
-- ID.
--
-- 'associationType', 'assistantAssociationData_associationType' - The type of association.
newAssistantAssociationData ::
  -- | 'assistantArn'
  Prelude.Text ->
  -- | 'assistantAssociationArn'
  Prelude.Text ->
  -- | 'assistantAssociationId'
  Prelude.Text ->
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'associationData'
  AssistantAssociationOutputData ->
  -- | 'associationType'
  AssociationType ->
  AssistantAssociationData
newAssistantAssociationData
  pAssistantArn_
  pAssistantAssociationArn_
  pAssistantAssociationId_
  pAssistantId_
  pAssociationData_
  pAssociationType_ =
    AssistantAssociationData'
      { tags = Prelude.Nothing,
        assistantArn = pAssistantArn_,
        assistantAssociationArn =
          pAssistantAssociationArn_,
        assistantAssociationId = pAssistantAssociationId_,
        assistantId = pAssistantId_,
        associationData = pAssociationData_,
        associationType = pAssociationType_
      }

-- | The tags used to organize, track, or control access for this resource.
assistantAssociationData_tags :: Lens.Lens' AssistantAssociationData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
assistantAssociationData_tags = Lens.lens (\AssistantAssociationData' {tags} -> tags) (\s@AssistantAssociationData' {} a -> s {tags = a} :: AssistantAssociationData) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Wisdom assistant.
assistantAssociationData_assistantArn :: Lens.Lens' AssistantAssociationData Prelude.Text
assistantAssociationData_assistantArn = Lens.lens (\AssistantAssociationData' {assistantArn} -> assistantArn) (\s@AssistantAssociationData' {} a -> s {assistantArn = a} :: AssistantAssociationData)

-- | The Amazon Resource Name (ARN) of the assistant association.
assistantAssociationData_assistantAssociationArn :: Lens.Lens' AssistantAssociationData Prelude.Text
assistantAssociationData_assistantAssociationArn = Lens.lens (\AssistantAssociationData' {assistantAssociationArn} -> assistantAssociationArn) (\s@AssistantAssociationData' {} a -> s {assistantAssociationArn = a} :: AssistantAssociationData)

-- | The identifier of the assistant association.
assistantAssociationData_assistantAssociationId :: Lens.Lens' AssistantAssociationData Prelude.Text
assistantAssociationData_assistantAssociationId = Lens.lens (\AssistantAssociationData' {assistantAssociationId} -> assistantAssociationId) (\s@AssistantAssociationData' {} a -> s {assistantAssociationId = a} :: AssistantAssociationData)

-- | The identifier of the Wisdom assistant.
assistantAssociationData_assistantId :: Lens.Lens' AssistantAssociationData Prelude.Text
assistantAssociationData_assistantId = Lens.lens (\AssistantAssociationData' {assistantId} -> assistantId) (\s@AssistantAssociationData' {} a -> s {assistantId = a} :: AssistantAssociationData)

-- | A union type that currently has a single argument, the knowledge base
-- ID.
assistantAssociationData_associationData :: Lens.Lens' AssistantAssociationData AssistantAssociationOutputData
assistantAssociationData_associationData = Lens.lens (\AssistantAssociationData' {associationData} -> associationData) (\s@AssistantAssociationData' {} a -> s {associationData = a} :: AssistantAssociationData)

-- | The type of association.
assistantAssociationData_associationType :: Lens.Lens' AssistantAssociationData AssociationType
assistantAssociationData_associationType = Lens.lens (\AssistantAssociationData' {associationType} -> associationType) (\s@AssistantAssociationData' {} a -> s {associationType = a} :: AssistantAssociationData)

instance Data.FromJSON AssistantAssociationData where
  parseJSON =
    Data.withObject
      "AssistantAssociationData"
      ( \x ->
          AssistantAssociationData'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "assistantArn")
            Prelude.<*> (x Data..: "assistantAssociationArn")
            Prelude.<*> (x Data..: "assistantAssociationId")
            Prelude.<*> (x Data..: "assistantId")
            Prelude.<*> (x Data..: "associationData")
            Prelude.<*> (x Data..: "associationType")
      )

instance Prelude.Hashable AssistantAssociationData where
  hashWithSalt _salt AssistantAssociationData' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assistantArn
      `Prelude.hashWithSalt` assistantAssociationArn
      `Prelude.hashWithSalt` assistantAssociationId
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` associationData
      `Prelude.hashWithSalt` associationType

instance Prelude.NFData AssistantAssociationData where
  rnf AssistantAssociationData' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf assistantArn
      `Prelude.seq` Prelude.rnf assistantAssociationArn
      `Prelude.seq` Prelude.rnf assistantAssociationId
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf associationData
      `Prelude.seq` Prelude.rnf associationType
