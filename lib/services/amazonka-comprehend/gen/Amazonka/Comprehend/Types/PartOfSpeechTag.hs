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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | Identifies the part of speech that the token represents.
    tag :: Prelude.Maybe PartOfSpeechTagType,
    -- | The confidence that Amazon Comprehend has that the part of speech was
    -- correctly identified.
    score :: Prelude.Maybe Prelude.Double
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
-- 'tag', 'partOfSpeechTag_tag' - Identifies the part of speech that the token represents.
--
-- 'score', 'partOfSpeechTag_score' - The confidence that Amazon Comprehend has that the part of speech was
-- correctly identified.
newPartOfSpeechTag ::
  PartOfSpeechTag
newPartOfSpeechTag =
  PartOfSpeechTag'
    { tag = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | Identifies the part of speech that the token represents.
partOfSpeechTag_tag :: Lens.Lens' PartOfSpeechTag (Prelude.Maybe PartOfSpeechTagType)
partOfSpeechTag_tag = Lens.lens (\PartOfSpeechTag' {tag} -> tag) (\s@PartOfSpeechTag' {} a -> s {tag = a} :: PartOfSpeechTag)

-- | The confidence that Amazon Comprehend has that the part of speech was
-- correctly identified.
partOfSpeechTag_score :: Lens.Lens' PartOfSpeechTag (Prelude.Maybe Prelude.Double)
partOfSpeechTag_score = Lens.lens (\PartOfSpeechTag' {score} -> score) (\s@PartOfSpeechTag' {} a -> s {score = a} :: PartOfSpeechTag)

instance Data.FromJSON PartOfSpeechTag where
  parseJSON =
    Data.withObject
      "PartOfSpeechTag"
      ( \x ->
          PartOfSpeechTag'
            Prelude.<$> (x Data..:? "Tag") Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable PartOfSpeechTag where
  hashWithSalt _salt PartOfSpeechTag' {..} =
    _salt `Prelude.hashWithSalt` tag
      `Prelude.hashWithSalt` score

instance Prelude.NFData PartOfSpeechTag where
  rnf PartOfSpeechTag' {..} =
    Prelude.rnf tag `Prelude.seq` Prelude.rnf score
