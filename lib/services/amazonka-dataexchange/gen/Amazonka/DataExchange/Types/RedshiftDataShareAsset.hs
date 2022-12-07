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
-- Module      : Amazonka.DataExchange.Types.RedshiftDataShareAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.RedshiftDataShareAsset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Redshift datashare asset.
--
-- /See:/ 'newRedshiftDataShareAsset' smart constructor.
data RedshiftDataShareAsset = RedshiftDataShareAsset'
  { -- | The Amazon Resource Name (ARN) of the datashare asset.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDataShareAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'redshiftDataShareAsset_arn' - The Amazon Resource Name (ARN) of the datashare asset.
newRedshiftDataShareAsset ::
  -- | 'arn'
  Prelude.Text ->
  RedshiftDataShareAsset
newRedshiftDataShareAsset pArn_ =
  RedshiftDataShareAsset' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the datashare asset.
redshiftDataShareAsset_arn :: Lens.Lens' RedshiftDataShareAsset Prelude.Text
redshiftDataShareAsset_arn = Lens.lens (\RedshiftDataShareAsset' {arn} -> arn) (\s@RedshiftDataShareAsset' {} a -> s {arn = a} :: RedshiftDataShareAsset)

instance Data.FromJSON RedshiftDataShareAsset where
  parseJSON =
    Data.withObject
      "RedshiftDataShareAsset"
      ( \x ->
          RedshiftDataShareAsset'
            Prelude.<$> (x Data..: "Arn")
      )

instance Prelude.Hashable RedshiftDataShareAsset where
  hashWithSalt _salt RedshiftDataShareAsset' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData RedshiftDataShareAsset where
  rnf RedshiftDataShareAsset' {..} = Prelude.rnf arn
