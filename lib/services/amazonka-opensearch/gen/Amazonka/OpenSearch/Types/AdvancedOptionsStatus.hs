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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdvancedOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | Status of the advanced options for the specified domain. The following
-- options are available:
--
-- -   @\"rest.action.multi.allow_explicit_index\": \"true\" | \"false\"@ -
--     Note the use of a string rather than a boolean. Specifies whether
--     explicit references to indexes are allowed inside the body of HTTP
--     requests. If you want to configure access policies for domain
--     sub-resources, such as specific indexes and domain APIs, you must
--     disable this property. Default is true.
--
-- -   @\"indices.fielddata.cache.size\": \"80\" @ - Note the use of a
--     string rather than a boolean. Specifies the percentage of heap space
--     allocated to field data. Default is unbounded.
--
-- -   @\"indices.query.bool.max_clause_count\": \"1024\"@ - Note the use
--     of a string rather than a boolean. Specifies the maximum number of
--     clauses allowed in a Lucene boolean query. Default is 1,024. Queries
--     with more than the permitted number of clauses result in a
--     @TooManyClauses@ error.
--
-- -   @\"override_main_response_version\": \"true\" | \"false\"@ - Note
--     the use of a string rather than a boolean. Specifies whether the
--     domain reports its version as 7.10 to allow Elasticsearch OSS
--     clients and plugins to continue working with it. Default is false
--     when creating a domain and true when upgrading a domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced cluster parameters>.
--
-- /See:/ 'newAdvancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
  { -- | The status of advanced options for the specified domain.
    options :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The status of advanced options for the specified domain.
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
-- 'status', 'advancedOptionsStatus_status' - The status of advanced options for the specified domain.
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

-- | The status of advanced options for the specified domain.
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

instance Prelude.Hashable AdvancedOptionsStatus where
  hashWithSalt _salt AdvancedOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData AdvancedOptionsStatus where
  rnf AdvancedOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
