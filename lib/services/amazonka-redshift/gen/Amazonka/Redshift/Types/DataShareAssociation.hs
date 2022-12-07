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
-- Module      : Amazonka.Redshift.Types.DataShareAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DataShareAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.DataShareStatus

-- | The association of a datashare from a producer account with a data
-- consumer.
--
-- /See:/ 'newDataShareAssociation' smart constructor.
data DataShareAssociation = DataShareAssociation'
  { -- | The name of the consumer accounts that have an association with a
    -- producer datashare.
    consumerIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The status change data of the datashare that is associated.
    statusChangeDate :: Prelude.Maybe Data.ISO8601,
    -- | The status of the datashare that is associated.
    status :: Prelude.Maybe DataShareStatus,
    -- | The creation date of the datashare that is associated.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Web Services Region of the consumer accounts that have an
    -- association with a producer datashare.
    consumerRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataShareAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerIdentifier', 'dataShareAssociation_consumerIdentifier' - The name of the consumer accounts that have an association with a
-- producer datashare.
--
-- 'statusChangeDate', 'dataShareAssociation_statusChangeDate' - The status change data of the datashare that is associated.
--
-- 'status', 'dataShareAssociation_status' - The status of the datashare that is associated.
--
-- 'createdDate', 'dataShareAssociation_createdDate' - The creation date of the datashare that is associated.
--
-- 'consumerRegion', 'dataShareAssociation_consumerRegion' - The Amazon Web Services Region of the consumer accounts that have an
-- association with a producer datashare.
newDataShareAssociation ::
  DataShareAssociation
newDataShareAssociation =
  DataShareAssociation'
    { consumerIdentifier =
        Prelude.Nothing,
      statusChangeDate = Prelude.Nothing,
      status = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      consumerRegion = Prelude.Nothing
    }

-- | The name of the consumer accounts that have an association with a
-- producer datashare.
dataShareAssociation_consumerIdentifier :: Lens.Lens' DataShareAssociation (Prelude.Maybe Prelude.Text)
dataShareAssociation_consumerIdentifier = Lens.lens (\DataShareAssociation' {consumerIdentifier} -> consumerIdentifier) (\s@DataShareAssociation' {} a -> s {consumerIdentifier = a} :: DataShareAssociation)

-- | The status change data of the datashare that is associated.
dataShareAssociation_statusChangeDate :: Lens.Lens' DataShareAssociation (Prelude.Maybe Prelude.UTCTime)
dataShareAssociation_statusChangeDate = Lens.lens (\DataShareAssociation' {statusChangeDate} -> statusChangeDate) (\s@DataShareAssociation' {} a -> s {statusChangeDate = a} :: DataShareAssociation) Prelude.. Lens.mapping Data._Time

-- | The status of the datashare that is associated.
dataShareAssociation_status :: Lens.Lens' DataShareAssociation (Prelude.Maybe DataShareStatus)
dataShareAssociation_status = Lens.lens (\DataShareAssociation' {status} -> status) (\s@DataShareAssociation' {} a -> s {status = a} :: DataShareAssociation)

-- | The creation date of the datashare that is associated.
dataShareAssociation_createdDate :: Lens.Lens' DataShareAssociation (Prelude.Maybe Prelude.UTCTime)
dataShareAssociation_createdDate = Lens.lens (\DataShareAssociation' {createdDate} -> createdDate) (\s@DataShareAssociation' {} a -> s {createdDate = a} :: DataShareAssociation) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services Region of the consumer accounts that have an
-- association with a producer datashare.
dataShareAssociation_consumerRegion :: Lens.Lens' DataShareAssociation (Prelude.Maybe Prelude.Text)
dataShareAssociation_consumerRegion = Lens.lens (\DataShareAssociation' {consumerRegion} -> consumerRegion) (\s@DataShareAssociation' {} a -> s {consumerRegion = a} :: DataShareAssociation)

instance Data.FromXML DataShareAssociation where
  parseXML x =
    DataShareAssociation'
      Prelude.<$> (x Data..@? "ConsumerIdentifier")
      Prelude.<*> (x Data..@? "StatusChangeDate")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "CreatedDate")
      Prelude.<*> (x Data..@? "ConsumerRegion")

instance Prelude.Hashable DataShareAssociation where
  hashWithSalt _salt DataShareAssociation' {..} =
    _salt `Prelude.hashWithSalt` consumerIdentifier
      `Prelude.hashWithSalt` statusChangeDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` consumerRegion

instance Prelude.NFData DataShareAssociation where
  rnf DataShareAssociation' {..} =
    Prelude.rnf consumerIdentifier
      `Prelude.seq` Prelude.rnf statusChangeDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf consumerRegion
