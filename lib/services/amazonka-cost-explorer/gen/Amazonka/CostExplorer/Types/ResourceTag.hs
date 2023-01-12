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
-- Module      : Amazonka.CostExplorer.Types.ResourceTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ResourceTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The tag structure that contains a tag key and value.
--
-- Tagging is supported only for the following Cost Explorer resource
-- types:
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalyMonitor.html AnomalyMonitor>
-- ,
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalySubscription.html AnomalySubscription>
-- ,
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategory.html CostCategory>
-- .
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The key that\'s associated with the tag.
    key :: Prelude.Text,
    -- | The value that\'s associated with the tag.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'resourceTag_key' - The key that\'s associated with the tag.
--
-- 'value', 'resourceTag_value' - The value that\'s associated with the tag.
newResourceTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  ResourceTag
newResourceTag pKey_ pValue_ =
  ResourceTag' {key = pKey_, value = pValue_}

-- | The key that\'s associated with the tag.
resourceTag_key :: Lens.Lens' ResourceTag Prelude.Text
resourceTag_key = Lens.lens (\ResourceTag' {key} -> key) (\s@ResourceTag' {} a -> s {key = a} :: ResourceTag)

-- | The value that\'s associated with the tag.
resourceTag_value :: Lens.Lens' ResourceTag Prelude.Text
resourceTag_value = Lens.lens (\ResourceTag' {value} -> value) (\s@ResourceTag' {} a -> s {value = a} :: ResourceTag)

instance Data.FromJSON ResourceTag where
  parseJSON =
    Data.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Prelude.<$> (x Data..: "Key") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable ResourceTag where
  hashWithSalt _salt ResourceTag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ResourceTag where
  rnf ResourceTag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
