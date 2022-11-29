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
-- Module      : Amazonka.CostExplorer.Types.UpdateCostAllocationTagsStatusError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.UpdateCostAllocationTagsStatusError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Gives a detailed description of the result of an action. It\'s on each
-- cost allocation tag entry in the request.
--
-- /See:/ 'newUpdateCostAllocationTagsStatusError' smart constructor.
data UpdateCostAllocationTagsStatusError = UpdateCostAllocationTagsStatusError'
  { -- | A message explaining why the action failed on this entry.
    message :: Prelude.Maybe Prelude.Text,
    -- | An error code representing why the action failed on this entry.
    code :: Prelude.Maybe Prelude.Text,
    -- | The key for the cost allocation tag.
    tagKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCostAllocationTagsStatusError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'updateCostAllocationTagsStatusError_message' - A message explaining why the action failed on this entry.
--
-- 'code', 'updateCostAllocationTagsStatusError_code' - An error code representing why the action failed on this entry.
--
-- 'tagKey', 'updateCostAllocationTagsStatusError_tagKey' - The key for the cost allocation tag.
newUpdateCostAllocationTagsStatusError ::
  UpdateCostAllocationTagsStatusError
newUpdateCostAllocationTagsStatusError =
  UpdateCostAllocationTagsStatusError'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing,
      tagKey = Prelude.Nothing
    }

-- | A message explaining why the action failed on this entry.
updateCostAllocationTagsStatusError_message :: Lens.Lens' UpdateCostAllocationTagsStatusError (Prelude.Maybe Prelude.Text)
updateCostAllocationTagsStatusError_message = Lens.lens (\UpdateCostAllocationTagsStatusError' {message} -> message) (\s@UpdateCostAllocationTagsStatusError' {} a -> s {message = a} :: UpdateCostAllocationTagsStatusError)

-- | An error code representing why the action failed on this entry.
updateCostAllocationTagsStatusError_code :: Lens.Lens' UpdateCostAllocationTagsStatusError (Prelude.Maybe Prelude.Text)
updateCostAllocationTagsStatusError_code = Lens.lens (\UpdateCostAllocationTagsStatusError' {code} -> code) (\s@UpdateCostAllocationTagsStatusError' {} a -> s {code = a} :: UpdateCostAllocationTagsStatusError)

-- | The key for the cost allocation tag.
updateCostAllocationTagsStatusError_tagKey :: Lens.Lens' UpdateCostAllocationTagsStatusError (Prelude.Maybe Prelude.Text)
updateCostAllocationTagsStatusError_tagKey = Lens.lens (\UpdateCostAllocationTagsStatusError' {tagKey} -> tagKey) (\s@UpdateCostAllocationTagsStatusError' {} a -> s {tagKey = a} :: UpdateCostAllocationTagsStatusError)

instance
  Core.FromJSON
    UpdateCostAllocationTagsStatusError
  where
  parseJSON =
    Core.withObject
      "UpdateCostAllocationTagsStatusError"
      ( \x ->
          UpdateCostAllocationTagsStatusError'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Code")
            Prelude.<*> (x Core..:? "TagKey")
      )

instance
  Prelude.Hashable
    UpdateCostAllocationTagsStatusError
  where
  hashWithSalt
    _salt
    UpdateCostAllocationTagsStatusError' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` tagKey

instance
  Prelude.NFData
    UpdateCostAllocationTagsStatusError
  where
  rnf UpdateCostAllocationTagsStatusError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf tagKey
