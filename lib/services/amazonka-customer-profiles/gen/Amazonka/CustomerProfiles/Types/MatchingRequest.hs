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
-- Module      : Amazonka.CustomerProfiles.Types.MatchingRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.MatchingRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The flag that enables the matching process of duplicate profiles.
--
-- /See:/ 'newMatchingRequest' smart constructor.
data MatchingRequest = MatchingRequest'
  { -- | The flag that enables the matching process of duplicate profiles.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchingRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'matchingRequest_enabled' - The flag that enables the matching process of duplicate profiles.
newMatchingRequest ::
  -- | 'enabled'
  Prelude.Bool ->
  MatchingRequest
newMatchingRequest pEnabled_ =
  MatchingRequest' {enabled = pEnabled_}

-- | The flag that enables the matching process of duplicate profiles.
matchingRequest_enabled :: Lens.Lens' MatchingRequest Prelude.Bool
matchingRequest_enabled = Lens.lens (\MatchingRequest' {enabled} -> enabled) (\s@MatchingRequest' {} a -> s {enabled = a} :: MatchingRequest)

instance Prelude.Hashable MatchingRequest

instance Prelude.NFData MatchingRequest

instance Core.ToJSON MatchingRequest where
  toJSON MatchingRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Enabled" Core..= enabled)]
      )
