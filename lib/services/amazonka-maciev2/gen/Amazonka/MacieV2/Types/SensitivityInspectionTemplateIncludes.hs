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
-- Module      : Amazonka.MacieV2.Types.SensitivityInspectionTemplateIncludes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SensitivityInspectionTemplateIncludes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the allow lists, custom data identifiers, and managed data
-- identifiers to include (use) when performing automated sensitive data
-- discovery for an Amazon Macie account. The configuration must specify at
-- least one custom data identifier or managed data identifier. For
-- information about the managed data identifiers that Amazon Macie
-- currently provides, see
-- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newSensitivityInspectionTemplateIncludes' smart constructor.
data SensitivityInspectionTemplateIncludes = SensitivityInspectionTemplateIncludes'
  { -- | An array of unique identifiers, one for each allow list to include.
    allowListIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of unique identifiers, one for each custom data identifier to
    -- include.
    customDataIdentifierIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of unique identifiers, one for each managed data identifier to
    -- include.
    --
    -- Amazon Macie uses these managed data identifiers in addition to managed
    -- data identifiers that are subsequently released and recommended for
    -- automated sensitive data discovery. To retrieve a list of valid values
    -- for the managed data identifiers that are currently available, use the
    -- ListManagedDataIdentifiers operation.
    managedDataIdentifierIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensitivityInspectionTemplateIncludes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowListIds', 'sensitivityInspectionTemplateIncludes_allowListIds' - An array of unique identifiers, one for each allow list to include.
--
-- 'customDataIdentifierIds', 'sensitivityInspectionTemplateIncludes_customDataIdentifierIds' - An array of unique identifiers, one for each custom data identifier to
-- include.
--
-- 'managedDataIdentifierIds', 'sensitivityInspectionTemplateIncludes_managedDataIdentifierIds' - An array of unique identifiers, one for each managed data identifier to
-- include.
--
-- Amazon Macie uses these managed data identifiers in addition to managed
-- data identifiers that are subsequently released and recommended for
-- automated sensitive data discovery. To retrieve a list of valid values
-- for the managed data identifiers that are currently available, use the
-- ListManagedDataIdentifiers operation.
newSensitivityInspectionTemplateIncludes ::
  SensitivityInspectionTemplateIncludes
newSensitivityInspectionTemplateIncludes =
  SensitivityInspectionTemplateIncludes'
    { allowListIds =
        Prelude.Nothing,
      customDataIdentifierIds =
        Prelude.Nothing,
      managedDataIdentifierIds =
        Prelude.Nothing
    }

-- | An array of unique identifiers, one for each allow list to include.
sensitivityInspectionTemplateIncludes_allowListIds :: Lens.Lens' SensitivityInspectionTemplateIncludes (Prelude.Maybe [Prelude.Text])
sensitivityInspectionTemplateIncludes_allowListIds = Lens.lens (\SensitivityInspectionTemplateIncludes' {allowListIds} -> allowListIds) (\s@SensitivityInspectionTemplateIncludes' {} a -> s {allowListIds = a} :: SensitivityInspectionTemplateIncludes) Prelude.. Lens.mapping Lens.coerced

-- | An array of unique identifiers, one for each custom data identifier to
-- include.
sensitivityInspectionTemplateIncludes_customDataIdentifierIds :: Lens.Lens' SensitivityInspectionTemplateIncludes (Prelude.Maybe [Prelude.Text])
sensitivityInspectionTemplateIncludes_customDataIdentifierIds = Lens.lens (\SensitivityInspectionTemplateIncludes' {customDataIdentifierIds} -> customDataIdentifierIds) (\s@SensitivityInspectionTemplateIncludes' {} a -> s {customDataIdentifierIds = a} :: SensitivityInspectionTemplateIncludes) Prelude.. Lens.mapping Lens.coerced

-- | An array of unique identifiers, one for each managed data identifier to
-- include.
--
-- Amazon Macie uses these managed data identifiers in addition to managed
-- data identifiers that are subsequently released and recommended for
-- automated sensitive data discovery. To retrieve a list of valid values
-- for the managed data identifiers that are currently available, use the
-- ListManagedDataIdentifiers operation.
sensitivityInspectionTemplateIncludes_managedDataIdentifierIds :: Lens.Lens' SensitivityInspectionTemplateIncludes (Prelude.Maybe [Prelude.Text])
sensitivityInspectionTemplateIncludes_managedDataIdentifierIds = Lens.lens (\SensitivityInspectionTemplateIncludes' {managedDataIdentifierIds} -> managedDataIdentifierIds) (\s@SensitivityInspectionTemplateIncludes' {} a -> s {managedDataIdentifierIds = a} :: SensitivityInspectionTemplateIncludes) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SensitivityInspectionTemplateIncludes
  where
  parseJSON =
    Data.withObject
      "SensitivityInspectionTemplateIncludes"
      ( \x ->
          SensitivityInspectionTemplateIncludes'
            Prelude.<$> (x Data..:? "allowListIds" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "customDataIdentifierIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "managedDataIdentifierIds"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    SensitivityInspectionTemplateIncludes
  where
  hashWithSalt
    _salt
    SensitivityInspectionTemplateIncludes' {..} =
      _salt
        `Prelude.hashWithSalt` allowListIds
        `Prelude.hashWithSalt` customDataIdentifierIds
        `Prelude.hashWithSalt` managedDataIdentifierIds

instance
  Prelude.NFData
    SensitivityInspectionTemplateIncludes
  where
  rnf SensitivityInspectionTemplateIncludes' {..} =
    Prelude.rnf allowListIds
      `Prelude.seq` Prelude.rnf customDataIdentifierIds
      `Prelude.seq` Prelude.rnf managedDataIdentifierIds

instance
  Data.ToJSON
    SensitivityInspectionTemplateIncludes
  where
  toJSON SensitivityInspectionTemplateIncludes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowListIds" Data..=) Prelude.<$> allowListIds,
            ("customDataIdentifierIds" Data..=)
              Prelude.<$> customDataIdentifierIds,
            ("managedDataIdentifierIds" Data..=)
              Prelude.<$> managedDataIdentifierIds
          ]
      )
