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
-- Module      : Amazonka.MacieV2.Types.SensitivityInspectionTemplateExcludes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SensitivityInspectionTemplateExcludes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies managed data identifiers to exclude (not use) when performing
-- automated sensitive data discovery for an Amazon Macie account. For
-- information about the managed data identifiers that Amazon Macie
-- currently provides, see
-- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newSensitivityInspectionTemplateExcludes' smart constructor.
data SensitivityInspectionTemplateExcludes = SensitivityInspectionTemplateExcludes'
  { -- | An array of unique identifiers, one for each managed data identifier to
    -- exclude. To retrieve a list of valid values, use the
    -- ListManagedDataIdentifiers operation.
    managedDataIdentifierIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensitivityInspectionTemplateExcludes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedDataIdentifierIds', 'sensitivityInspectionTemplateExcludes_managedDataIdentifierIds' - An array of unique identifiers, one for each managed data identifier to
-- exclude. To retrieve a list of valid values, use the
-- ListManagedDataIdentifiers operation.
newSensitivityInspectionTemplateExcludes ::
  SensitivityInspectionTemplateExcludes
newSensitivityInspectionTemplateExcludes =
  SensitivityInspectionTemplateExcludes'
    { managedDataIdentifierIds =
        Prelude.Nothing
    }

-- | An array of unique identifiers, one for each managed data identifier to
-- exclude. To retrieve a list of valid values, use the
-- ListManagedDataIdentifiers operation.
sensitivityInspectionTemplateExcludes_managedDataIdentifierIds :: Lens.Lens' SensitivityInspectionTemplateExcludes (Prelude.Maybe [Prelude.Text])
sensitivityInspectionTemplateExcludes_managedDataIdentifierIds = Lens.lens (\SensitivityInspectionTemplateExcludes' {managedDataIdentifierIds} -> managedDataIdentifierIds) (\s@SensitivityInspectionTemplateExcludes' {} a -> s {managedDataIdentifierIds = a} :: SensitivityInspectionTemplateExcludes) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SensitivityInspectionTemplateExcludes
  where
  parseJSON =
    Data.withObject
      "SensitivityInspectionTemplateExcludes"
      ( \x ->
          SensitivityInspectionTemplateExcludes'
            Prelude.<$> ( x
                            Data..:? "managedDataIdentifierIds"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    SensitivityInspectionTemplateExcludes
  where
  hashWithSalt
    _salt
    SensitivityInspectionTemplateExcludes' {..} =
      _salt
        `Prelude.hashWithSalt` managedDataIdentifierIds

instance
  Prelude.NFData
    SensitivityInspectionTemplateExcludes
  where
  rnf SensitivityInspectionTemplateExcludes' {..} =
    Prelude.rnf managedDataIdentifierIds

instance
  Data.ToJSON
    SensitivityInspectionTemplateExcludes
  where
  toJSON SensitivityInspectionTemplateExcludes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("managedDataIdentifierIds" Data..=)
              Prelude.<$> managedDataIdentifierIds
          ]
      )
