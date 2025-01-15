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
-- Module      : Amazonka.OpenSearch.Types.AdvancedSecurityOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdvancedSecurityOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AdvancedSecurityOptions
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of fine-grained access control settings for a domain.
--
-- /See:/ 'newAdvancedSecurityOptionsStatus' smart constructor.
data AdvancedSecurityOptionsStatus = AdvancedSecurityOptionsStatus'
  { -- | Container for fine-grained access control settings.
    options :: AdvancedSecurityOptions,
    -- | Status of the fine-grained access control settings for a domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedSecurityOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'advancedSecurityOptionsStatus_options' - Container for fine-grained access control settings.
--
-- 'status', 'advancedSecurityOptionsStatus_status' - Status of the fine-grained access control settings for a domain.
newAdvancedSecurityOptionsStatus ::
  -- | 'options'
  AdvancedSecurityOptions ->
  -- | 'status'
  OptionStatus ->
  AdvancedSecurityOptionsStatus
newAdvancedSecurityOptionsStatus pOptions_ pStatus_ =
  AdvancedSecurityOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Container for fine-grained access control settings.
advancedSecurityOptionsStatus_options :: Lens.Lens' AdvancedSecurityOptionsStatus AdvancedSecurityOptions
advancedSecurityOptionsStatus_options = Lens.lens (\AdvancedSecurityOptionsStatus' {options} -> options) (\s@AdvancedSecurityOptionsStatus' {} a -> s {options = a} :: AdvancedSecurityOptionsStatus)

-- | Status of the fine-grained access control settings for a domain.
advancedSecurityOptionsStatus_status :: Lens.Lens' AdvancedSecurityOptionsStatus OptionStatus
advancedSecurityOptionsStatus_status = Lens.lens (\AdvancedSecurityOptionsStatus' {status} -> status) (\s@AdvancedSecurityOptionsStatus' {} a -> s {status = a} :: AdvancedSecurityOptionsStatus)

instance Data.FromJSON AdvancedSecurityOptionsStatus where
  parseJSON =
    Data.withObject
      "AdvancedSecurityOptionsStatus"
      ( \x ->
          AdvancedSecurityOptionsStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    AdvancedSecurityOptionsStatus
  where
  hashWithSalt _salt AdvancedSecurityOptionsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData AdvancedSecurityOptionsStatus where
  rnf AdvancedSecurityOptionsStatus' {..} =
    Prelude.rnf options `Prelude.seq`
      Prelude.rnf status
