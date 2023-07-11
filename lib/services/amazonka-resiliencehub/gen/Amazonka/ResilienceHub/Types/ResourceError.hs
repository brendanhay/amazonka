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
-- Module      : Amazonka.ResilienceHub.Types.ResourceError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResourceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines application resource errors.
--
-- /See:/ 'newResourceError' smart constructor.
data ResourceError = ResourceError'
  { -- | This is the identifier of the resource.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | This is the identifier of the physical resource.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | This is the error message.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalResourceId', 'resourceError_logicalResourceId' - This is the identifier of the resource.
--
-- 'physicalResourceId', 'resourceError_physicalResourceId' - This is the identifier of the physical resource.
--
-- 'reason', 'resourceError_reason' - This is the error message.
newResourceError ::
  ResourceError
newResourceError =
  ResourceError'
    { logicalResourceId = Prelude.Nothing,
      physicalResourceId = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | This is the identifier of the resource.
resourceError_logicalResourceId :: Lens.Lens' ResourceError (Prelude.Maybe Prelude.Text)
resourceError_logicalResourceId = Lens.lens (\ResourceError' {logicalResourceId} -> logicalResourceId) (\s@ResourceError' {} a -> s {logicalResourceId = a} :: ResourceError)

-- | This is the identifier of the physical resource.
resourceError_physicalResourceId :: Lens.Lens' ResourceError (Prelude.Maybe Prelude.Text)
resourceError_physicalResourceId = Lens.lens (\ResourceError' {physicalResourceId} -> physicalResourceId) (\s@ResourceError' {} a -> s {physicalResourceId = a} :: ResourceError)

-- | This is the error message.
resourceError_reason :: Lens.Lens' ResourceError (Prelude.Maybe Prelude.Text)
resourceError_reason = Lens.lens (\ResourceError' {reason} -> reason) (\s@ResourceError' {} a -> s {reason = a} :: ResourceError)

instance Data.FromJSON ResourceError where
  parseJSON =
    Data.withObject
      "ResourceError"
      ( \x ->
          ResourceError'
            Prelude.<$> (x Data..:? "logicalResourceId")
            Prelude.<*> (x Data..:? "physicalResourceId")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable ResourceError where
  hashWithSalt _salt ResourceError' {..} =
    _salt
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ResourceError where
  rnf ResourceError' {..} =
    Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf reason
