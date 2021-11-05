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
-- Module      : Amazonka.OpenSearch.Types.AdvancedOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdvancedOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | Status of the advanced options for the specified domain. Currently, the
-- following advanced options are available:
--
-- -   Option to allow references to indices in an HTTP request body. Must
--     be @false@ when configuring access to individual sub-resources. By
--     default, the value is @true@. See
--     <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>
--     for more information.
-- -   Option to specify the percentage of heap space allocated to field
--     data. By default, this setting is unbounded.
--
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>.
--
-- /See:/ 'newAdvancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
  { -- | The status of advanced options for the specified domain.
    options :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The @OptionStatus@ for advanced options for the specified domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'advancedOptionsStatus_options' - The status of advanced options for the specified domain.
--
-- 'status', 'advancedOptionsStatus_status' - The @OptionStatus@ for advanced options for the specified domain.
newAdvancedOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  AdvancedOptionsStatus
newAdvancedOptionsStatus pStatus_ =
  AdvancedOptionsStatus'
    { options = Prelude.mempty,
      status = pStatus_
    }

-- | The status of advanced options for the specified domain.
advancedOptionsStatus_options :: Lens.Lens' AdvancedOptionsStatus (Prelude.HashMap Prelude.Text Prelude.Text)
advancedOptionsStatus_options = Lens.lens (\AdvancedOptionsStatus' {options} -> options) (\s@AdvancedOptionsStatus' {} a -> s {options = a} :: AdvancedOptionsStatus) Prelude.. Lens.coerced

-- | The @OptionStatus@ for advanced options for the specified domain.
advancedOptionsStatus_status :: Lens.Lens' AdvancedOptionsStatus OptionStatus
advancedOptionsStatus_status = Lens.lens (\AdvancedOptionsStatus' {status} -> status) (\s@AdvancedOptionsStatus' {} a -> s {status = a} :: AdvancedOptionsStatus)

instance Core.FromJSON AdvancedOptionsStatus where
  parseJSON =
    Core.withObject
      "AdvancedOptionsStatus"
      ( \x ->
          AdvancedOptionsStatus'
            Prelude.<$> (x Core..:? "Options" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable AdvancedOptionsStatus

instance Prelude.NFData AdvancedOptionsStatus
