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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterResizeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterResizeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the resize operation for the cluster.
--
-- /See:/ 'newAwsRedshiftClusterResizeInfo' smart constructor.
data AwsRedshiftClusterResizeInfo = AwsRedshiftClusterResizeInfo'
  { -- | Indicates whether the resize operation can be canceled.
    allowCancelResize :: Prelude.Maybe Prelude.Bool,
    -- | The type of resize operation.
    --
    -- Valid values: @ClassicResize@
    resizeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterResizeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowCancelResize', 'awsRedshiftClusterResizeInfo_allowCancelResize' - Indicates whether the resize operation can be canceled.
--
-- 'resizeType', 'awsRedshiftClusterResizeInfo_resizeType' - The type of resize operation.
--
-- Valid values: @ClassicResize@
newAwsRedshiftClusterResizeInfo ::
  AwsRedshiftClusterResizeInfo
newAwsRedshiftClusterResizeInfo =
  AwsRedshiftClusterResizeInfo'
    { allowCancelResize =
        Prelude.Nothing,
      resizeType = Prelude.Nothing
    }

-- | Indicates whether the resize operation can be canceled.
awsRedshiftClusterResizeInfo_allowCancelResize :: Lens.Lens' AwsRedshiftClusterResizeInfo (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterResizeInfo_allowCancelResize = Lens.lens (\AwsRedshiftClusterResizeInfo' {allowCancelResize} -> allowCancelResize) (\s@AwsRedshiftClusterResizeInfo' {} a -> s {allowCancelResize = a} :: AwsRedshiftClusterResizeInfo)

-- | The type of resize operation.
--
-- Valid values: @ClassicResize@
awsRedshiftClusterResizeInfo_resizeType :: Lens.Lens' AwsRedshiftClusterResizeInfo (Prelude.Maybe Prelude.Text)
awsRedshiftClusterResizeInfo_resizeType = Lens.lens (\AwsRedshiftClusterResizeInfo' {resizeType} -> resizeType) (\s@AwsRedshiftClusterResizeInfo' {} a -> s {resizeType = a} :: AwsRedshiftClusterResizeInfo)

instance Data.FromJSON AwsRedshiftClusterResizeInfo where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterResizeInfo"
      ( \x ->
          AwsRedshiftClusterResizeInfo'
            Prelude.<$> (x Data..:? "AllowCancelResize")
            Prelude.<*> (x Data..:? "ResizeType")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterResizeInfo
  where
  hashWithSalt _salt AwsRedshiftClusterResizeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` allowCancelResize
      `Prelude.hashWithSalt` resizeType

instance Prelude.NFData AwsRedshiftClusterResizeInfo where
  rnf AwsRedshiftClusterResizeInfo' {..} =
    Prelude.rnf allowCancelResize
      `Prelude.seq` Prelude.rnf resizeType

instance Data.ToJSON AwsRedshiftClusterResizeInfo where
  toJSON AwsRedshiftClusterResizeInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowCancelResize" Data..=)
              Prelude.<$> allowCancelResize,
            ("ResizeType" Data..=) Prelude.<$> resizeType
          ]
      )
