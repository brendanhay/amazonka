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
-- Module      : Amazonka.Wisdom.Types.AssistantAssociationOutputData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AssistantAssociationOutputData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.KnowledgeBaseAssociationData

-- | The data that is output as a result of the assistant association.
--
-- /See:/ 'newAssistantAssociationOutputData' smart constructor.
data AssistantAssociationOutputData = AssistantAssociationOutputData'
  { -- | The knowledge base where output data is sent.
    knowledgeBaseAssociation :: Prelude.Maybe KnowledgeBaseAssociationData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssistantAssociationOutputData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBaseAssociation', 'assistantAssociationOutputData_knowledgeBaseAssociation' - The knowledge base where output data is sent.
newAssistantAssociationOutputData ::
  AssistantAssociationOutputData
newAssistantAssociationOutputData =
  AssistantAssociationOutputData'
    { knowledgeBaseAssociation =
        Prelude.Nothing
    }

-- | The knowledge base where output data is sent.
assistantAssociationOutputData_knowledgeBaseAssociation :: Lens.Lens' AssistantAssociationOutputData (Prelude.Maybe KnowledgeBaseAssociationData)
assistantAssociationOutputData_knowledgeBaseAssociation = Lens.lens (\AssistantAssociationOutputData' {knowledgeBaseAssociation} -> knowledgeBaseAssociation) (\s@AssistantAssociationOutputData' {} a -> s {knowledgeBaseAssociation = a} :: AssistantAssociationOutputData)

instance Core.FromJSON AssistantAssociationOutputData where
  parseJSON =
    Core.withObject
      "AssistantAssociationOutputData"
      ( \x ->
          AssistantAssociationOutputData'
            Prelude.<$> (x Core..:? "knowledgeBaseAssociation")
      )

instance
  Prelude.Hashable
    AssistantAssociationOutputData
  where
  hashWithSalt
    _salt
    AssistantAssociationOutputData' {..} =
      _salt
        `Prelude.hashWithSalt` knowledgeBaseAssociation

instance
  Prelude.NFData
    AssistantAssociationOutputData
  where
  rnf AssistantAssociationOutputData' {..} =
    Prelude.rnf knowledgeBaseAssociation
