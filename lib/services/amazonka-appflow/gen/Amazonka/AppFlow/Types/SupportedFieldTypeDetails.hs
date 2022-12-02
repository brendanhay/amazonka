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
-- Module      : Amazonka.AppFlow.Types.SupportedFieldTypeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SupportedFieldTypeDetails where

import Amazonka.AppFlow.Types.FieldTypeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details regarding all the supported @FieldTypes@ and their
-- corresponding @filterOperators@ and @supportedValues@.
--
-- /See:/ 'newSupportedFieldTypeDetails' smart constructor.
data SupportedFieldTypeDetails = SupportedFieldTypeDetails'
  { -- | The initial supported version for @fieldType@. If this is later changed
    -- to a different version, v2 will be introduced.
    v1 :: FieldTypeDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedFieldTypeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'v1', 'supportedFieldTypeDetails_v1' - The initial supported version for @fieldType@. If this is later changed
-- to a different version, v2 will be introduced.
newSupportedFieldTypeDetails ::
  -- | 'v1'
  FieldTypeDetails ->
  SupportedFieldTypeDetails
newSupportedFieldTypeDetails pV1_ =
  SupportedFieldTypeDetails' {v1 = pV1_}

-- | The initial supported version for @fieldType@. If this is later changed
-- to a different version, v2 will be introduced.
supportedFieldTypeDetails_v1 :: Lens.Lens' SupportedFieldTypeDetails FieldTypeDetails
supportedFieldTypeDetails_v1 = Lens.lens (\SupportedFieldTypeDetails' {v1} -> v1) (\s@SupportedFieldTypeDetails' {} a -> s {v1 = a} :: SupportedFieldTypeDetails)

instance Data.FromJSON SupportedFieldTypeDetails where
  parseJSON =
    Data.withObject
      "SupportedFieldTypeDetails"
      ( \x ->
          SupportedFieldTypeDetails'
            Prelude.<$> (x Data..: "v1")
      )

instance Prelude.Hashable SupportedFieldTypeDetails where
  hashWithSalt _salt SupportedFieldTypeDetails' {..} =
    _salt `Prelude.hashWithSalt` v1

instance Prelude.NFData SupportedFieldTypeDetails where
  rnf SupportedFieldTypeDetails' {..} = Prelude.rnf v1
