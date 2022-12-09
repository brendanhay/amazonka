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
-- Module      : Amazonka.FinSpaceData.Types.DatasetOwnerInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.DatasetOwnerInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for Dataset owner info.
--
-- /See:/ 'newDatasetOwnerInfo' smart constructor.
data DatasetOwnerInfo = DatasetOwnerInfo'
  { -- | Email address for the Dataset owner.
    email :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the Dataset owner.
    name :: Prelude.Maybe Prelude.Text,
    -- | Phone number for the Dataset owner.
    phoneNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetOwnerInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'datasetOwnerInfo_email' - Email address for the Dataset owner.
--
-- 'name', 'datasetOwnerInfo_name' - The name of the Dataset owner.
--
-- 'phoneNumber', 'datasetOwnerInfo_phoneNumber' - Phone number for the Dataset owner.
newDatasetOwnerInfo ::
  DatasetOwnerInfo
newDatasetOwnerInfo =
  DatasetOwnerInfo'
    { email = Prelude.Nothing,
      name = Prelude.Nothing,
      phoneNumber = Prelude.Nothing
    }

-- | Email address for the Dataset owner.
datasetOwnerInfo_email :: Lens.Lens' DatasetOwnerInfo (Prelude.Maybe Prelude.Text)
datasetOwnerInfo_email = Lens.lens (\DatasetOwnerInfo' {email} -> email) (\s@DatasetOwnerInfo' {} a -> s {email = a} :: DatasetOwnerInfo) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the Dataset owner.
datasetOwnerInfo_name :: Lens.Lens' DatasetOwnerInfo (Prelude.Maybe Prelude.Text)
datasetOwnerInfo_name = Lens.lens (\DatasetOwnerInfo' {name} -> name) (\s@DatasetOwnerInfo' {} a -> s {name = a} :: DatasetOwnerInfo)

-- | Phone number for the Dataset owner.
datasetOwnerInfo_phoneNumber :: Lens.Lens' DatasetOwnerInfo (Prelude.Maybe Prelude.Text)
datasetOwnerInfo_phoneNumber = Lens.lens (\DatasetOwnerInfo' {phoneNumber} -> phoneNumber) (\s@DatasetOwnerInfo' {} a -> s {phoneNumber = a} :: DatasetOwnerInfo)

instance Data.FromJSON DatasetOwnerInfo where
  parseJSON =
    Data.withObject
      "DatasetOwnerInfo"
      ( \x ->
          DatasetOwnerInfo'
            Prelude.<$> (x Data..:? "email")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "phoneNumber")
      )

instance Prelude.Hashable DatasetOwnerInfo where
  hashWithSalt _salt DatasetOwnerInfo' {..} =
    _salt `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData DatasetOwnerInfo where
  rnf DatasetOwnerInfo' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf phoneNumber

instance Data.ToJSON DatasetOwnerInfo where
  toJSON DatasetOwnerInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("email" Data..=) Prelude.<$> email,
            ("name" Data..=) Prelude.<$> name,
            ("phoneNumber" Data..=) Prelude.<$> phoneNumber
          ]
      )
