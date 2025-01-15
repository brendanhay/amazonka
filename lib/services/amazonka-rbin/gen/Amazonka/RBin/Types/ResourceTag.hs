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
-- Module      : Amazonka.RBin.Types.ResourceTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types.ResourceTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The tag value.
    resourceTagValue :: Prelude.Maybe Prelude.Text,
    -- | The tag key.
    resourceTagKey :: Prelude.Text
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
-- 'resourceTagValue', 'resourceTag_resourceTagValue' - The tag value.
--
-- 'resourceTagKey', 'resourceTag_resourceTagKey' - The tag key.
newResourceTag ::
  -- | 'resourceTagKey'
  Prelude.Text ->
  ResourceTag
newResourceTag pResourceTagKey_ =
  ResourceTag'
    { resourceTagValue = Prelude.Nothing,
      resourceTagKey = pResourceTagKey_
    }

-- | The tag value.
resourceTag_resourceTagValue :: Lens.Lens' ResourceTag (Prelude.Maybe Prelude.Text)
resourceTag_resourceTagValue = Lens.lens (\ResourceTag' {resourceTagValue} -> resourceTagValue) (\s@ResourceTag' {} a -> s {resourceTagValue = a} :: ResourceTag)

-- | The tag key.
resourceTag_resourceTagKey :: Lens.Lens' ResourceTag Prelude.Text
resourceTag_resourceTagKey = Lens.lens (\ResourceTag' {resourceTagKey} -> resourceTagKey) (\s@ResourceTag' {} a -> s {resourceTagKey = a} :: ResourceTag)

instance Data.FromJSON ResourceTag where
  parseJSON =
    Data.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Prelude.<$> (x Data..:? "ResourceTagValue")
            Prelude.<*> (x Data..: "ResourceTagKey")
      )

instance Prelude.Hashable ResourceTag where
  hashWithSalt _salt ResourceTag' {..} =
    _salt
      `Prelude.hashWithSalt` resourceTagValue
      `Prelude.hashWithSalt` resourceTagKey

instance Prelude.NFData ResourceTag where
  rnf ResourceTag' {..} =
    Prelude.rnf resourceTagValue `Prelude.seq`
      Prelude.rnf resourceTagKey

instance Data.ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceTagValue" Data..=)
              Prelude.<$> resourceTagValue,
            Prelude.Just
              ("ResourceTagKey" Data..= resourceTagKey)
          ]
      )
