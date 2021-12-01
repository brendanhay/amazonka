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
-- Module      : Amazonka.MacieV2.Types.TagCriterionPairForJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.TagCriterionPairForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag key, a tag value, or a tag key and value (as a pair) to
-- use in a tag-based condition that determines whether an S3 bucket is
-- included or excluded from a classification job. Tag keys and values are
-- case sensitive. Also, Amazon Macie doesn\'t support use of partial
-- values or wildcard characters in tag-based conditions.
--
-- /See:/ 'newTagCriterionPairForJob' smart constructor.
data TagCriterionPairForJob = TagCriterionPairForJob'
  { -- | The tag value to use in the condition.
    value :: Prelude.Maybe Prelude.Text,
    -- | The value for the tag key to use in the condition.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagCriterionPairForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tagCriterionPairForJob_value' - The tag value to use in the condition.
--
-- 'key', 'tagCriterionPairForJob_key' - The value for the tag key to use in the condition.
newTagCriterionPairForJob ::
  TagCriterionPairForJob
newTagCriterionPairForJob =
  TagCriterionPairForJob'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The tag value to use in the condition.
tagCriterionPairForJob_value :: Lens.Lens' TagCriterionPairForJob (Prelude.Maybe Prelude.Text)
tagCriterionPairForJob_value = Lens.lens (\TagCriterionPairForJob' {value} -> value) (\s@TagCriterionPairForJob' {} a -> s {value = a} :: TagCriterionPairForJob)

-- | The value for the tag key to use in the condition.
tagCriterionPairForJob_key :: Lens.Lens' TagCriterionPairForJob (Prelude.Maybe Prelude.Text)
tagCriterionPairForJob_key = Lens.lens (\TagCriterionPairForJob' {key} -> key) (\s@TagCriterionPairForJob' {} a -> s {key = a} :: TagCriterionPairForJob)

instance Core.FromJSON TagCriterionPairForJob where
  parseJSON =
    Core.withObject
      "TagCriterionPairForJob"
      ( \x ->
          TagCriterionPairForJob'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable TagCriterionPairForJob where
  hashWithSalt salt' TagCriterionPairForJob' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagCriterionPairForJob where
  rnf TagCriterionPairForJob' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Core.ToJSON TagCriterionPairForJob where
  toJSON TagCriterionPairForJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("key" Core..=) Prelude.<$> key
          ]
      )
