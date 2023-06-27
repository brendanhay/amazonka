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
-- Module      : Amazonka.Location.Types.ApiKeyFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ApiKeyFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | Options for filtering API keys.
--
-- /See:/ 'newApiKeyFilter' smart constructor.
data ApiKeyFilter = ApiKeyFilter'
  { -- | Filter on @Active@ or @Expired@ API keys.
    keyStatus :: Prelude.Maybe Status
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiKeyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyStatus', 'apiKeyFilter_keyStatus' - Filter on @Active@ or @Expired@ API keys.
newApiKeyFilter ::
  ApiKeyFilter
newApiKeyFilter =
  ApiKeyFilter' {keyStatus = Prelude.Nothing}

-- | Filter on @Active@ or @Expired@ API keys.
apiKeyFilter_keyStatus :: Lens.Lens' ApiKeyFilter (Prelude.Maybe Status)
apiKeyFilter_keyStatus = Lens.lens (\ApiKeyFilter' {keyStatus} -> keyStatus) (\s@ApiKeyFilter' {} a -> s {keyStatus = a} :: ApiKeyFilter)

instance Prelude.Hashable ApiKeyFilter where
  hashWithSalt _salt ApiKeyFilter' {..} =
    _salt `Prelude.hashWithSalt` keyStatus

instance Prelude.NFData ApiKeyFilter where
  rnf ApiKeyFilter' {..} = Prelude.rnf keyStatus

instance Data.ToJSON ApiKeyFilter where
  toJSON ApiKeyFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("KeyStatus" Data..=) Prelude.<$> keyStatus]
      )
