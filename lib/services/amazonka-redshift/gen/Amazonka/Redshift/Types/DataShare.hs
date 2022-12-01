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
-- Module      : Amazonka.Redshift.Types.DataShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DataShare where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.DataShareAssociation

-- | /See:/ 'newDataShare' smart constructor.
data DataShare = DataShare'
  { -- | A value that specifies when the datashare has an association between
    -- producer and data consumers.
    dataShareAssociations :: Prelude.Maybe [DataShareAssociation],
    -- | The Amazon Resource Name (ARN) of the producer.
    producerArn :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies whether the datashare can be shared to a publicly
    -- accessible cluster.
    allowPubliclyAccessibleConsumers :: Prelude.Maybe Prelude.Bool,
    -- | An Amazon Resource Name (ARN) that references the datashare that is
    -- owned by a specific namespace of the producer cluster. A datashare ARN
    -- is in the
    -- @arn:aws:redshift:{region}:{account-id}:{datashare}:{namespace-guid}\/{datashare-name}@
    -- format.
    dataShareArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a datashare to show its managing entity.
    managedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShareAssociations', 'dataShare_dataShareAssociations' - A value that specifies when the datashare has an association between
-- producer and data consumers.
--
-- 'producerArn', 'dataShare_producerArn' - The Amazon Resource Name (ARN) of the producer.
--
-- 'allowPubliclyAccessibleConsumers', 'dataShare_allowPubliclyAccessibleConsumers' - A value that specifies whether the datashare can be shared to a publicly
-- accessible cluster.
--
-- 'dataShareArn', 'dataShare_dataShareArn' - An Amazon Resource Name (ARN) that references the datashare that is
-- owned by a specific namespace of the producer cluster. A datashare ARN
-- is in the
-- @arn:aws:redshift:{region}:{account-id}:{datashare}:{namespace-guid}\/{datashare-name}@
-- format.
--
-- 'managedBy', 'dataShare_managedBy' - The identifier of a datashare to show its managing entity.
newDataShare ::
  DataShare
newDataShare =
  DataShare'
    { dataShareAssociations = Prelude.Nothing,
      producerArn = Prelude.Nothing,
      allowPubliclyAccessibleConsumers = Prelude.Nothing,
      dataShareArn = Prelude.Nothing,
      managedBy = Prelude.Nothing
    }

-- | A value that specifies when the datashare has an association between
-- producer and data consumers.
dataShare_dataShareAssociations :: Lens.Lens' DataShare (Prelude.Maybe [DataShareAssociation])
dataShare_dataShareAssociations = Lens.lens (\DataShare' {dataShareAssociations} -> dataShareAssociations) (\s@DataShare' {} a -> s {dataShareAssociations = a} :: DataShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the producer.
dataShare_producerArn :: Lens.Lens' DataShare (Prelude.Maybe Prelude.Text)
dataShare_producerArn = Lens.lens (\DataShare' {producerArn} -> producerArn) (\s@DataShare' {} a -> s {producerArn = a} :: DataShare)

-- | A value that specifies whether the datashare can be shared to a publicly
-- accessible cluster.
dataShare_allowPubliclyAccessibleConsumers :: Lens.Lens' DataShare (Prelude.Maybe Prelude.Bool)
dataShare_allowPubliclyAccessibleConsumers = Lens.lens (\DataShare' {allowPubliclyAccessibleConsumers} -> allowPubliclyAccessibleConsumers) (\s@DataShare' {} a -> s {allowPubliclyAccessibleConsumers = a} :: DataShare)

-- | An Amazon Resource Name (ARN) that references the datashare that is
-- owned by a specific namespace of the producer cluster. A datashare ARN
-- is in the
-- @arn:aws:redshift:{region}:{account-id}:{datashare}:{namespace-guid}\/{datashare-name}@
-- format.
dataShare_dataShareArn :: Lens.Lens' DataShare (Prelude.Maybe Prelude.Text)
dataShare_dataShareArn = Lens.lens (\DataShare' {dataShareArn} -> dataShareArn) (\s@DataShare' {} a -> s {dataShareArn = a} :: DataShare)

-- | The identifier of a datashare to show its managing entity.
dataShare_managedBy :: Lens.Lens' DataShare (Prelude.Maybe Prelude.Text)
dataShare_managedBy = Lens.lens (\DataShare' {managedBy} -> managedBy) (\s@DataShare' {} a -> s {managedBy = a} :: DataShare)

instance Core.FromXML DataShare where
  parseXML x =
    DataShare'
      Prelude.<$> ( x Core..@? "DataShareAssociations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ProducerArn")
      Prelude.<*> (x Core..@? "AllowPubliclyAccessibleConsumers")
      Prelude.<*> (x Core..@? "DataShareArn")
      Prelude.<*> (x Core..@? "ManagedBy")

instance Prelude.Hashable DataShare where
  hashWithSalt _salt DataShare' {..} =
    _salt `Prelude.hashWithSalt` dataShareAssociations
      `Prelude.hashWithSalt` producerArn
      `Prelude.hashWithSalt` allowPubliclyAccessibleConsumers
      `Prelude.hashWithSalt` dataShareArn
      `Prelude.hashWithSalt` managedBy

instance Prelude.NFData DataShare where
  rnf DataShare' {..} =
    Prelude.rnf dataShareAssociations
      `Prelude.seq` Prelude.rnf producerArn
      `Prelude.seq` Prelude.rnf allowPubliclyAccessibleConsumers
      `Prelude.seq` Prelude.rnf dataShareArn
      `Prelude.seq` Prelude.rnf managedBy
