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
-- Module      : Amazonka.SecurityLake.Types.AwsLogSourceResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AwsLogSourceResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsLogSourceName

-- | Amazon Security Lake can collect logs and events from natively-supported
-- Amazon Web Services services.
--
-- /See:/ 'newAwsLogSourceResource' smart constructor.
data AwsLogSourceResource = AwsLogSourceResource'
  { -- | The name for a Amazon Web Services source. This must be a Regionally
    -- unique value.
    sourceName :: Prelude.Maybe AwsLogSourceName,
    -- | The version for a Amazon Web Services source. This must be a Regionally
    -- unique value.
    sourceVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLogSourceResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceName', 'awsLogSourceResource_sourceName' - The name for a Amazon Web Services source. This must be a Regionally
-- unique value.
--
-- 'sourceVersion', 'awsLogSourceResource_sourceVersion' - The version for a Amazon Web Services source. This must be a Regionally
-- unique value.
newAwsLogSourceResource ::
  AwsLogSourceResource
newAwsLogSourceResource =
  AwsLogSourceResource'
    { sourceName = Prelude.Nothing,
      sourceVersion = Prelude.Nothing
    }

-- | The name for a Amazon Web Services source. This must be a Regionally
-- unique value.
awsLogSourceResource_sourceName :: Lens.Lens' AwsLogSourceResource (Prelude.Maybe AwsLogSourceName)
awsLogSourceResource_sourceName = Lens.lens (\AwsLogSourceResource' {sourceName} -> sourceName) (\s@AwsLogSourceResource' {} a -> s {sourceName = a} :: AwsLogSourceResource)

-- | The version for a Amazon Web Services source. This must be a Regionally
-- unique value.
awsLogSourceResource_sourceVersion :: Lens.Lens' AwsLogSourceResource (Prelude.Maybe Prelude.Text)
awsLogSourceResource_sourceVersion = Lens.lens (\AwsLogSourceResource' {sourceVersion} -> sourceVersion) (\s@AwsLogSourceResource' {} a -> s {sourceVersion = a} :: AwsLogSourceResource)

instance Data.FromJSON AwsLogSourceResource where
  parseJSON =
    Data.withObject
      "AwsLogSourceResource"
      ( \x ->
          AwsLogSourceResource'
            Prelude.<$> (x Data..:? "sourceName")
            Prelude.<*> (x Data..:? "sourceVersion")
      )

instance Prelude.Hashable AwsLogSourceResource where
  hashWithSalt _salt AwsLogSourceResource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` sourceVersion

instance Prelude.NFData AwsLogSourceResource where
  rnf AwsLogSourceResource' {..} =
    Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf sourceVersion

instance Data.ToJSON AwsLogSourceResource where
  toJSON AwsLogSourceResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceName" Data..=) Prelude.<$> sourceName,
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion
          ]
      )
