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
-- Module      : Amazonka.Comprehend.Types.PartOfSpeechTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PartOfSpeechTag where

import Amazonka.Comprehend.Types.PartOfSpeechTagType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the part of speech represented by the token and gives the
-- confidence that Amazon Comprehend has that the part of speech was
-- correctly identified. For more information about the parts of speech
-- that Amazon Comprehend can identify, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-syntax.html Syntax>
-- in the Comprehend Developer Guide.
--
-- /See:/ 'newPartOfSpeechTag' smart constructor.
data PartOfSpeechTag = PartOfSpeechTag'
  { -- | The confidence that Amazon Comprehend has that the part of speech was
    -- correctly identified.
    score :: Prelude.Maybe Prelude.Double,
    -- | Identifies the part of speech that the token represents.
    tag :: Prelude.Maybe PartOfSpeechTagType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartOfSpeechTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'partOfSpeechTag_score' - The confidence that Amazon Comprehend has that the part of speech was
-- correctly identified.
--
-- 'tag', 'partOfSpeechTag_tag' - Identifies the part of speech that the token represents.
newPartOfSpeechTag ::
  PartOfSpeechTag
newPartOfSpeechTag =
  PartOfSpeechTag'
    { score = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | The confidence that Amazon Comprehend has that the part of speech was
-- correctly identified.
partOfSpeechTag_score :: Lens.Lens' PartOfSpeechTag (Prelude.Maybe Prelude.Double)
partOfSpeechTag_score = Lens.lens (\PartOfSpeechTag' {score} -> score) (\s@PartOfSpeechTag' {} a -> s {score = a} :: PartOfSpeechTag)

-- | Identifies the part of speech that the token represents.
partOfSpeechTag_tag :: Lens.Lens' PartOfSpeechTag (Prelude.Maybe PartOfSpeechTagType)
partOfSpeechTag_tag = Lens.lens (\PartOfSpeechTag' {tag} -> tag) (\s@PartOfSpeechTag' {} a -> s {tag = a} :: PartOfSpeechTag)

instance Data.FromJSON PartOfSpeechTag where
  parseJSON =
    Data.withObject
      "PartOfSpeechTag"
      ( \x ->
          PartOfSpeechTag'
            Prelude.<$> (x Data..:? "Score") Prelude.<*> (x Data..:? "Tag")
      )

instance Prelude.Hashable PartOfSpeechTag where
  hashWithSalt _salt PartOfSpeechTag' {..} =
    _salt `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` tag

instance Prelude.NFData PartOfSpeechTag where
  rnf PartOfSpeechTag' {..} =
    Prelude.rnf score `Prelude.seq` Prelude.rnf tag
