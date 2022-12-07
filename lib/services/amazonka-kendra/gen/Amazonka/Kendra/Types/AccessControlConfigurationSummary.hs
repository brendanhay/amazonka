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
-- Module      : Amazonka.Kendra.Types.AccessControlConfigurationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AccessControlConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information on an access control configuration that you created
-- for your documents in an index.
--
-- /See:/ 'newAccessControlConfigurationSummary' smart constructor.
data AccessControlConfigurationSummary = AccessControlConfigurationSummary'
  { -- | The identifier of the access control configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'accessControlConfigurationSummary_id' - The identifier of the access control configuration.
newAccessControlConfigurationSummary ::
  -- | 'id'
  Prelude.Text ->
  AccessControlConfigurationSummary
newAccessControlConfigurationSummary pId_ =
  AccessControlConfigurationSummary' {id = pId_}

-- | The identifier of the access control configuration.
accessControlConfigurationSummary_id :: Lens.Lens' AccessControlConfigurationSummary Prelude.Text
accessControlConfigurationSummary_id = Lens.lens (\AccessControlConfigurationSummary' {id} -> id) (\s@AccessControlConfigurationSummary' {} a -> s {id = a} :: AccessControlConfigurationSummary)

instance
  Data.FromJSON
    AccessControlConfigurationSummary
  where
  parseJSON =
    Data.withObject
      "AccessControlConfigurationSummary"
      ( \x ->
          AccessControlConfigurationSummary'
            Prelude.<$> (x Data..: "Id")
      )

instance
  Prelude.Hashable
    AccessControlConfigurationSummary
  where
  hashWithSalt
    _salt
    AccessControlConfigurationSummary' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    AccessControlConfigurationSummary
  where
  rnf AccessControlConfigurationSummary' {..} =
    Prelude.rnf id
