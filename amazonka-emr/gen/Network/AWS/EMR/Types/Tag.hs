{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Tag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key-value pair containing user-defined metadata that you can associate
-- with an Amazon EMR resource. Tags make it easier to associate clusters
-- in various ways, such as grouping clusters to track your Amazon EMR
-- resource allocation costs. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A user-defined key, which is the minimum required information for a
    -- valid tag. For more information, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag>
    -- .
    key :: Prelude.Maybe Prelude.Text,
    -- | A user-defined value, which is optional in a tag. For more information,
    -- see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tag_key' - A user-defined key, which is the minimum required information for a
-- valid tag. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag>
-- .
--
-- 'value', 'tag_value' - A user-defined value, which is optional in a tag. For more information,
-- see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
newTag ::
  Tag
newTag =
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A user-defined key, which is the minimum required information for a
-- valid tag. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag>
-- .
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | A user-defined value, which is optional in a tag. For more information,
-- see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Prelude.FromJSON Tag where
  parseJSON =
    Prelude.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToJSON Tag where
  toJSON Tag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
