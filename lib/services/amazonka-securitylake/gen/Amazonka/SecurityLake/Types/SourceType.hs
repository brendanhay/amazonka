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
-- Module      : Amazonka.SecurityLake.Types.SourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SourceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsLogSourceType

-- | The supported source types from which logs and events are collected in
-- Amazon Security Lake. For the list of supported Amazon Web Services, see
-- the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
--
-- /See:/ 'newSourceType' smart constructor.
data SourceType = SourceType'
  { -- | Amazon Security Lake supports log and event collection for natively
    -- supported Amazon Web Services.
    awsSourceType :: Prelude.Maybe AwsLogSourceType,
    -- | Amazon Security Lake supports custom source types. For a detailed list,
    -- see the Amazon Security Lake User Guide.
    customSourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsSourceType', 'sourceType_awsSourceType' - Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
--
-- 'customSourceType', 'sourceType_customSourceType' - Amazon Security Lake supports custom source types. For a detailed list,
-- see the Amazon Security Lake User Guide.
newSourceType ::
  SourceType
newSourceType =
  SourceType'
    { awsSourceType = Prelude.Nothing,
      customSourceType = Prelude.Nothing
    }

-- | Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
sourceType_awsSourceType :: Lens.Lens' SourceType (Prelude.Maybe AwsLogSourceType)
sourceType_awsSourceType = Lens.lens (\SourceType' {awsSourceType} -> awsSourceType) (\s@SourceType' {} a -> s {awsSourceType = a} :: SourceType)

-- | Amazon Security Lake supports custom source types. For a detailed list,
-- see the Amazon Security Lake User Guide.
sourceType_customSourceType :: Lens.Lens' SourceType (Prelude.Maybe Prelude.Text)
sourceType_customSourceType = Lens.lens (\SourceType' {customSourceType} -> customSourceType) (\s@SourceType' {} a -> s {customSourceType = a} :: SourceType)

instance Data.FromJSON SourceType where
  parseJSON =
    Data.withObject
      "SourceType"
      ( \x ->
          SourceType'
            Prelude.<$> (x Data..:? "awsSourceType")
            Prelude.<*> (x Data..:? "customSourceType")
      )

instance Prelude.Hashable SourceType where
  hashWithSalt _salt SourceType' {..} =
    _salt
      `Prelude.hashWithSalt` awsSourceType
      `Prelude.hashWithSalt` customSourceType

instance Prelude.NFData SourceType where
  rnf SourceType' {..} =
    Prelude.rnf awsSourceType
      `Prelude.seq` Prelude.rnf customSourceType

instance Data.ToJSON SourceType where
  toJSON SourceType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsSourceType" Data..=) Prelude.<$> awsSourceType,
            ("customSourceType" Data..=)
              Prelude.<$> customSourceType
          ]
      )
