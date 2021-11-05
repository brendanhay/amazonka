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
-- Module      : Amazonka.CustomerProfiles.Types.MatchingResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.MatchingResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The flag that enables the matching process of duplicate profiles.
--
-- /See:/ 'newMatchingResponse' smart constructor.
data MatchingResponse = MatchingResponse'
  { -- | The flag that enables the matching process of duplicate profiles.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'matchingResponse_enabled' - The flag that enables the matching process of duplicate profiles.
newMatchingResponse ::
  MatchingResponse
newMatchingResponse =
  MatchingResponse' {enabled = Prelude.Nothing}

-- | The flag that enables the matching process of duplicate profiles.
matchingResponse_enabled :: Lens.Lens' MatchingResponse (Prelude.Maybe Prelude.Bool)
matchingResponse_enabled = Lens.lens (\MatchingResponse' {enabled} -> enabled) (\s@MatchingResponse' {} a -> s {enabled = a} :: MatchingResponse)

instance Core.FromJSON MatchingResponse where
  parseJSON =
    Core.withObject
      "MatchingResponse"
      ( \x ->
          MatchingResponse' Prelude.<$> (x Core..:? "Enabled")
      )

instance Prelude.Hashable MatchingResponse

instance Prelude.NFData MatchingResponse
