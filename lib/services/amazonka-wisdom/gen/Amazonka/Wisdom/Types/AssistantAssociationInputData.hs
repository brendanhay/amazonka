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
-- Module      : Amazonka.Wisdom.Types.AssistantAssociationInputData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AssistantAssociationInputData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data that is input into Wisdom as a result of the assistant
-- association.
--
-- /See:/ 'newAssistantAssociationInputData' smart constructor.
data AssistantAssociationInputData = AssistantAssociationInputData'
  { -- | The identifier of the knowledge base.
    knowledgeBaseId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssistantAssociationInputData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBaseId', 'assistantAssociationInputData_knowledgeBaseId' - The identifier of the knowledge base.
newAssistantAssociationInputData ::
  AssistantAssociationInputData
newAssistantAssociationInputData =
  AssistantAssociationInputData'
    { knowledgeBaseId =
        Prelude.Nothing
    }

-- | The identifier of the knowledge base.
assistantAssociationInputData_knowledgeBaseId :: Lens.Lens' AssistantAssociationInputData (Prelude.Maybe Prelude.Text)
assistantAssociationInputData_knowledgeBaseId = Lens.lens (\AssistantAssociationInputData' {knowledgeBaseId} -> knowledgeBaseId) (\s@AssistantAssociationInputData' {} a -> s {knowledgeBaseId = a} :: AssistantAssociationInputData)

instance
  Prelude.Hashable
    AssistantAssociationInputData
  where
  hashWithSalt _salt AssistantAssociationInputData' {..} =
    _salt `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData AssistantAssociationInputData where
  rnf AssistantAssociationInputData' {..} =
    Prelude.rnf knowledgeBaseId

instance Data.ToJSON AssistantAssociationInputData where
  toJSON AssistantAssociationInputData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("knowledgeBaseId" Data..=)
              Prelude.<$> knowledgeBaseId
          ]
      )
