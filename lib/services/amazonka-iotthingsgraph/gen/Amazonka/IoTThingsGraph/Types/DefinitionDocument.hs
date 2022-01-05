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
-- Module      : Amazonka.IoTThingsGraph.Types.DefinitionDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.DefinitionDocument where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.DefinitionLanguage
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A document that defines an entity.
--
-- /See:/ 'newDefinitionDocument' smart constructor.
data DefinitionDocument = DefinitionDocument'
  { -- | The language used to define the entity. @GRAPHQL@ is the only valid
    -- value.
    language :: DefinitionLanguage,
    -- | The GraphQL text that defines the entity.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefinitionDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'definitionDocument_language' - The language used to define the entity. @GRAPHQL@ is the only valid
-- value.
--
-- 'text', 'definitionDocument_text' - The GraphQL text that defines the entity.
newDefinitionDocument ::
  -- | 'language'
  DefinitionLanguage ->
  -- | 'text'
  Prelude.Text ->
  DefinitionDocument
newDefinitionDocument pLanguage_ pText_ =
  DefinitionDocument'
    { language = pLanguage_,
      text = pText_
    }

-- | The language used to define the entity. @GRAPHQL@ is the only valid
-- value.
definitionDocument_language :: Lens.Lens' DefinitionDocument DefinitionLanguage
definitionDocument_language = Lens.lens (\DefinitionDocument' {language} -> language) (\s@DefinitionDocument' {} a -> s {language = a} :: DefinitionDocument)

-- | The GraphQL text that defines the entity.
definitionDocument_text :: Lens.Lens' DefinitionDocument Prelude.Text
definitionDocument_text = Lens.lens (\DefinitionDocument' {text} -> text) (\s@DefinitionDocument' {} a -> s {text = a} :: DefinitionDocument)

instance Core.FromJSON DefinitionDocument where
  parseJSON =
    Core.withObject
      "DefinitionDocument"
      ( \x ->
          DefinitionDocument'
            Prelude.<$> (x Core..: "language")
            Prelude.<*> (x Core..: "text")
      )

instance Prelude.Hashable DefinitionDocument where
  hashWithSalt _salt DefinitionDocument' {..} =
    _salt `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` text

instance Prelude.NFData DefinitionDocument where
  rnf DefinitionDocument' {..} =
    Prelude.rnf language `Prelude.seq` Prelude.rnf text

instance Core.ToJSON DefinitionDocument where
  toJSON DefinitionDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("language" Core..= language),
            Prelude.Just ("text" Core..= text)
          ]
      )
