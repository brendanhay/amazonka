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
-- Module      : Amazonka.Wisdom.Types.AssistantAssociationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AssistantAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.AssistantAssociationOutputData
import Amazonka.Wisdom.Types.AssociationType

-- | Summary information about the assistant association.
--
-- /See:/ 'newAssistantAssociationSummary' smart constructor.
data AssistantAssociationSummary = AssistantAssociationSummary'
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
    -- | The association data.
    associationData :: AssistantAssociationOutputData,
    -- | The type of association.
    associationType :: AssociationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssistantAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'assistantAssociationSummary_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'assistantArn', 'assistantAssociationSummary_assistantArn' - The Amazon Resource Name (ARN) of the Wisdom assistant.
--
-- 'assistantAssociationArn', 'assistantAssociationSummary_assistantAssociationArn' - The Amazon Resource Name (ARN) of the assistant association.
--
-- 'assistantAssociationId', 'assistantAssociationSummary_assistantAssociationId' - The identifier of the assistant association.
--
-- 'assistantId', 'assistantAssociationSummary_assistantId' - The identifier of the Wisdom assistant.
--
-- 'associationData', 'assistantAssociationSummary_associationData' - The association data.
--
-- 'associationType', 'assistantAssociationSummary_associationType' - The type of association.
newAssistantAssociationSummary ::
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
  AssistantAssociationSummary
newAssistantAssociationSummary
  pAssistantArn_
  pAssistantAssociationArn_
  pAssistantAssociationId_
  pAssistantId_
  pAssociationData_
  pAssociationType_ =
    AssistantAssociationSummary'
      { tags =
          Prelude.Nothing,
        assistantArn = pAssistantArn_,
        assistantAssociationArn =
          pAssistantAssociationArn_,
        assistantAssociationId =
          pAssistantAssociationId_,
        assistantId = pAssistantId_,
        associationData = pAssociationData_,
        associationType = pAssociationType_
      }

-- | The tags used to organize, track, or control access for this resource.
assistantAssociationSummary_tags :: Lens.Lens' AssistantAssociationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
assistantAssociationSummary_tags = Lens.lens (\AssistantAssociationSummary' {tags} -> tags) (\s@AssistantAssociationSummary' {} a -> s {tags = a} :: AssistantAssociationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Wisdom assistant.
assistantAssociationSummary_assistantArn :: Lens.Lens' AssistantAssociationSummary Prelude.Text
assistantAssociationSummary_assistantArn = Lens.lens (\AssistantAssociationSummary' {assistantArn} -> assistantArn) (\s@AssistantAssociationSummary' {} a -> s {assistantArn = a} :: AssistantAssociationSummary)

-- | The Amazon Resource Name (ARN) of the assistant association.
assistantAssociationSummary_assistantAssociationArn :: Lens.Lens' AssistantAssociationSummary Prelude.Text
assistantAssociationSummary_assistantAssociationArn = Lens.lens (\AssistantAssociationSummary' {assistantAssociationArn} -> assistantAssociationArn) (\s@AssistantAssociationSummary' {} a -> s {assistantAssociationArn = a} :: AssistantAssociationSummary)

-- | The identifier of the assistant association.
assistantAssociationSummary_assistantAssociationId :: Lens.Lens' AssistantAssociationSummary Prelude.Text
assistantAssociationSummary_assistantAssociationId = Lens.lens (\AssistantAssociationSummary' {assistantAssociationId} -> assistantAssociationId) (\s@AssistantAssociationSummary' {} a -> s {assistantAssociationId = a} :: AssistantAssociationSummary)

-- | The identifier of the Wisdom assistant.
assistantAssociationSummary_assistantId :: Lens.Lens' AssistantAssociationSummary Prelude.Text
assistantAssociationSummary_assistantId = Lens.lens (\AssistantAssociationSummary' {assistantId} -> assistantId) (\s@AssistantAssociationSummary' {} a -> s {assistantId = a} :: AssistantAssociationSummary)

-- | The association data.
assistantAssociationSummary_associationData :: Lens.Lens' AssistantAssociationSummary AssistantAssociationOutputData
assistantAssociationSummary_associationData = Lens.lens (\AssistantAssociationSummary' {associationData} -> associationData) (\s@AssistantAssociationSummary' {} a -> s {associationData = a} :: AssistantAssociationSummary)

-- | The type of association.
assistantAssociationSummary_associationType :: Lens.Lens' AssistantAssociationSummary AssociationType
assistantAssociationSummary_associationType = Lens.lens (\AssistantAssociationSummary' {associationType} -> associationType) (\s@AssistantAssociationSummary' {} a -> s {associationType = a} :: AssistantAssociationSummary)

instance Data.FromJSON AssistantAssociationSummary where
  parseJSON =
    Data.withObject
      "AssistantAssociationSummary"
      ( \x ->
          AssistantAssociationSummary'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "assistantArn")
            Prelude.<*> (x Data..: "assistantAssociationArn")
            Prelude.<*> (x Data..: "assistantAssociationId")
            Prelude.<*> (x Data..: "assistantId")
            Prelude.<*> (x Data..: "associationData")
            Prelude.<*> (x Data..: "associationType")
      )

instance Prelude.Hashable AssistantAssociationSummary where
  hashWithSalt _salt AssistantAssociationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assistantArn
      `Prelude.hashWithSalt` assistantAssociationArn
      `Prelude.hashWithSalt` assistantAssociationId
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` associationData
      `Prelude.hashWithSalt` associationType

instance Prelude.NFData AssistantAssociationSummary where
  rnf AssistantAssociationSummary' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf assistantArn `Prelude.seq`
        Prelude.rnf assistantAssociationArn `Prelude.seq`
          Prelude.rnf assistantAssociationId `Prelude.seq`
            Prelude.rnf assistantId `Prelude.seq`
              Prelude.rnf associationData `Prelude.seq`
                Prelude.rnf associationType
