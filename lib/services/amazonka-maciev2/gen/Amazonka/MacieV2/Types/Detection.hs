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
-- Module      : Amazonka.MacieV2.Types.Detection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Detection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.DataIdentifierType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a type of sensitive data that Amazon Macie
-- found in an S3 bucket while performing automated sensitive data
-- discovery for the bucket. The information also specifies the custom data
-- identifier or managed data identifier that detected the data. This
-- information is available only if automated sensitive data discovery is
-- currently enabled for your account.
--
-- /See:/ 'newDetection' smart constructor.
data Detection = Detection'
  { -- | If the sensitive data was detected by a custom data identifier, the
    -- Amazon Resource Name (ARN) of the custom data identifier that detected
    -- the data. Otherwise, this value is null.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The total number of occurrences of the sensitive data.
    count :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier for the custom data identifier or managed data
    -- identifier that detected the sensitive data. For additional details
    -- about a specified managed data identifier, see
    -- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
    -- in the /Amazon Macie User Guide/.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom data identifier or managed data identifier that
    -- detected the sensitive data. For a managed data identifier, this value
    -- is the same as the unique identifier (id).
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether occurrences of this type of sensitive data are
    -- excluded (true) or included (false) in the bucket\'s sensitivity score.
    suppressed :: Prelude.Maybe Prelude.Bool,
    -- | The type of data identifier that detected the sensitive data. Possible
    -- values are: CUSTOM, for a custom data identifier; and, MANAGED, for a
    -- managed data identifier.
    type' :: Prelude.Maybe DataIdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Detection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'detection_arn' - If the sensitive data was detected by a custom data identifier, the
-- Amazon Resource Name (ARN) of the custom data identifier that detected
-- the data. Otherwise, this value is null.
--
-- 'count', 'detection_count' - The total number of occurrences of the sensitive data.
--
-- 'id', 'detection_id' - The unique identifier for the custom data identifier or managed data
-- identifier that detected the sensitive data. For additional details
-- about a specified managed data identifier, see
-- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
-- in the /Amazon Macie User Guide/.
--
-- 'name', 'detection_name' - The name of the custom data identifier or managed data identifier that
-- detected the sensitive data. For a managed data identifier, this value
-- is the same as the unique identifier (id).
--
-- 'suppressed', 'detection_suppressed' - Specifies whether occurrences of this type of sensitive data are
-- excluded (true) or included (false) in the bucket\'s sensitivity score.
--
-- 'type'', 'detection_type' - The type of data identifier that detected the sensitive data. Possible
-- values are: CUSTOM, for a custom data identifier; and, MANAGED, for a
-- managed data identifier.
newDetection ::
  Detection
newDetection =
  Detection'
    { arn = Prelude.Nothing,
      count = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      suppressed = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | If the sensitive data was detected by a custom data identifier, the
-- Amazon Resource Name (ARN) of the custom data identifier that detected
-- the data. Otherwise, this value is null.
detection_arn :: Lens.Lens' Detection (Prelude.Maybe Prelude.Text)
detection_arn = Lens.lens (\Detection' {arn} -> arn) (\s@Detection' {} a -> s {arn = a} :: Detection)

-- | The total number of occurrences of the sensitive data.
detection_count :: Lens.Lens' Detection (Prelude.Maybe Prelude.Integer)
detection_count = Lens.lens (\Detection' {count} -> count) (\s@Detection' {} a -> s {count = a} :: Detection)

-- | The unique identifier for the custom data identifier or managed data
-- identifier that detected the sensitive data. For additional details
-- about a specified managed data identifier, see
-- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
-- in the /Amazon Macie User Guide/.
detection_id :: Lens.Lens' Detection (Prelude.Maybe Prelude.Text)
detection_id = Lens.lens (\Detection' {id} -> id) (\s@Detection' {} a -> s {id = a} :: Detection)

-- | The name of the custom data identifier or managed data identifier that
-- detected the sensitive data. For a managed data identifier, this value
-- is the same as the unique identifier (id).
detection_name :: Lens.Lens' Detection (Prelude.Maybe Prelude.Text)
detection_name = Lens.lens (\Detection' {name} -> name) (\s@Detection' {} a -> s {name = a} :: Detection)

-- | Specifies whether occurrences of this type of sensitive data are
-- excluded (true) or included (false) in the bucket\'s sensitivity score.
detection_suppressed :: Lens.Lens' Detection (Prelude.Maybe Prelude.Bool)
detection_suppressed = Lens.lens (\Detection' {suppressed} -> suppressed) (\s@Detection' {} a -> s {suppressed = a} :: Detection)

-- | The type of data identifier that detected the sensitive data. Possible
-- values are: CUSTOM, for a custom data identifier; and, MANAGED, for a
-- managed data identifier.
detection_type :: Lens.Lens' Detection (Prelude.Maybe DataIdentifierType)
detection_type = Lens.lens (\Detection' {type'} -> type') (\s@Detection' {} a -> s {type' = a} :: Detection)

instance Data.FromJSON Detection where
  parseJSON =
    Data.withObject
      "Detection"
      ( \x ->
          Detection'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "count")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "suppressed")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Detection where
  hashWithSalt _salt Detection' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` suppressed
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Detection where
  rnf Detection' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf suppressed
      `Prelude.seq` Prelude.rnf type'
