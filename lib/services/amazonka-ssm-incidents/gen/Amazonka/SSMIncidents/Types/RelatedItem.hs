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
-- Module      : Amazonka.SSMIncidents.Types.RelatedItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.RelatedItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.ItemIdentifier

-- | Resources that responders use to triage and mitigate the incident.
--
-- /See:/ 'newRelatedItem' smart constructor.
data RelatedItem = RelatedItem'
  { -- | A unique ID for a @RelatedItem@.
    --
    -- Don\'t specify this parameter when you add a @RelatedItem@ by using the
    -- UpdateRelatedItems API action.
    generatedId :: Prelude.Maybe Prelude.Text,
    -- | The title of the related item.
    title :: Prelude.Maybe Prelude.Text,
    -- | Details about the related item.
    identifier :: ItemIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generatedId', 'relatedItem_generatedId' - A unique ID for a @RelatedItem@.
--
-- Don\'t specify this parameter when you add a @RelatedItem@ by using the
-- UpdateRelatedItems API action.
--
-- 'title', 'relatedItem_title' - The title of the related item.
--
-- 'identifier', 'relatedItem_identifier' - Details about the related item.
newRelatedItem ::
  -- | 'identifier'
  ItemIdentifier ->
  RelatedItem
newRelatedItem pIdentifier_ =
  RelatedItem'
    { generatedId = Prelude.Nothing,
      title = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | A unique ID for a @RelatedItem@.
--
-- Don\'t specify this parameter when you add a @RelatedItem@ by using the
-- UpdateRelatedItems API action.
relatedItem_generatedId :: Lens.Lens' RelatedItem (Prelude.Maybe Prelude.Text)
relatedItem_generatedId = Lens.lens (\RelatedItem' {generatedId} -> generatedId) (\s@RelatedItem' {} a -> s {generatedId = a} :: RelatedItem)

-- | The title of the related item.
relatedItem_title :: Lens.Lens' RelatedItem (Prelude.Maybe Prelude.Text)
relatedItem_title = Lens.lens (\RelatedItem' {title} -> title) (\s@RelatedItem' {} a -> s {title = a} :: RelatedItem)

-- | Details about the related item.
relatedItem_identifier :: Lens.Lens' RelatedItem ItemIdentifier
relatedItem_identifier = Lens.lens (\RelatedItem' {identifier} -> identifier) (\s@RelatedItem' {} a -> s {identifier = a} :: RelatedItem)

instance Data.FromJSON RelatedItem where
  parseJSON =
    Data.withObject
      "RelatedItem"
      ( \x ->
          RelatedItem'
            Prelude.<$> (x Data..:? "generatedId")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..: "identifier")
      )

instance Prelude.Hashable RelatedItem where
  hashWithSalt _salt RelatedItem' {..} =
    _salt
      `Prelude.hashWithSalt` generatedId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData RelatedItem where
  rnf RelatedItem' {..} =
    Prelude.rnf generatedId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToJSON RelatedItem where
  toJSON RelatedItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("generatedId" Data..=) Prelude.<$> generatedId,
            ("title" Data..=) Prelude.<$> title,
            Prelude.Just ("identifier" Data..= identifier)
          ]
      )
