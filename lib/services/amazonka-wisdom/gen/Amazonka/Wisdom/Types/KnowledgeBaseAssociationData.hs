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
-- Module      : Amazonka.Wisdom.Types.KnowledgeBaseAssociationData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.KnowledgeBaseAssociationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Association information about the knowledge base.
--
-- /See:/ 'newKnowledgeBaseAssociationData' smart constructor.
data KnowledgeBaseAssociationData = KnowledgeBaseAssociationData'
  { -- | The Amazon Resource Name (ARN) of the knowledge base.
    knowledgeBaseArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the knowledge base.
    knowledgeBaseId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KnowledgeBaseAssociationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBaseArn', 'knowledgeBaseAssociationData_knowledgeBaseArn' - The Amazon Resource Name (ARN) of the knowledge base.
--
-- 'knowledgeBaseId', 'knowledgeBaseAssociationData_knowledgeBaseId' - The identifier of the knowledge base.
newKnowledgeBaseAssociationData ::
  KnowledgeBaseAssociationData
newKnowledgeBaseAssociationData =
  KnowledgeBaseAssociationData'
    { knowledgeBaseArn =
        Prelude.Nothing,
      knowledgeBaseId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the knowledge base.
knowledgeBaseAssociationData_knowledgeBaseArn :: Lens.Lens' KnowledgeBaseAssociationData (Prelude.Maybe Prelude.Text)
knowledgeBaseAssociationData_knowledgeBaseArn = Lens.lens (\KnowledgeBaseAssociationData' {knowledgeBaseArn} -> knowledgeBaseArn) (\s@KnowledgeBaseAssociationData' {} a -> s {knowledgeBaseArn = a} :: KnowledgeBaseAssociationData)

-- | The identifier of the knowledge base.
knowledgeBaseAssociationData_knowledgeBaseId :: Lens.Lens' KnowledgeBaseAssociationData (Prelude.Maybe Prelude.Text)
knowledgeBaseAssociationData_knowledgeBaseId = Lens.lens (\KnowledgeBaseAssociationData' {knowledgeBaseId} -> knowledgeBaseId) (\s@KnowledgeBaseAssociationData' {} a -> s {knowledgeBaseId = a} :: KnowledgeBaseAssociationData)

instance Data.FromJSON KnowledgeBaseAssociationData where
  parseJSON =
    Data.withObject
      "KnowledgeBaseAssociationData"
      ( \x ->
          KnowledgeBaseAssociationData'
            Prelude.<$> (x Data..:? "knowledgeBaseArn")
            Prelude.<*> (x Data..:? "knowledgeBaseId")
      )

instance
  Prelude.Hashable
    KnowledgeBaseAssociationData
  where
  hashWithSalt _salt KnowledgeBaseAssociationData' {..} =
    _salt `Prelude.hashWithSalt` knowledgeBaseArn
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData KnowledgeBaseAssociationData where
  rnf KnowledgeBaseAssociationData' {..} =
    Prelude.rnf knowledgeBaseArn
      `Prelude.seq` Prelude.rnf knowledgeBaseId
