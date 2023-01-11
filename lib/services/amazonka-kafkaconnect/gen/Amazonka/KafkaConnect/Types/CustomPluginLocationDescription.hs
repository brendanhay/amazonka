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
-- Module      : Amazonka.KafkaConnect.Types.CustomPluginLocationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CustomPluginLocationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.S3LocationDescription
import qualified Amazonka.Prelude as Prelude

-- | Information about the location of a custom plugin.
--
-- /See:/ 'newCustomPluginLocationDescription' smart constructor.
data CustomPluginLocationDescription = CustomPluginLocationDescription'
  { -- | The S3 bucket Amazon Resource Name (ARN), file key, and object version
    -- of the plugin file stored in Amazon S3.
    s3Location :: Prelude.Maybe S3LocationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPluginLocationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'customPluginLocationDescription_s3Location' - The S3 bucket Amazon Resource Name (ARN), file key, and object version
-- of the plugin file stored in Amazon S3.
newCustomPluginLocationDescription ::
  CustomPluginLocationDescription
newCustomPluginLocationDescription =
  CustomPluginLocationDescription'
    { s3Location =
        Prelude.Nothing
    }

-- | The S3 bucket Amazon Resource Name (ARN), file key, and object version
-- of the plugin file stored in Amazon S3.
customPluginLocationDescription_s3Location :: Lens.Lens' CustomPluginLocationDescription (Prelude.Maybe S3LocationDescription)
customPluginLocationDescription_s3Location = Lens.lens (\CustomPluginLocationDescription' {s3Location} -> s3Location) (\s@CustomPluginLocationDescription' {} a -> s {s3Location = a} :: CustomPluginLocationDescription)

instance
  Data.FromJSON
    CustomPluginLocationDescription
  where
  parseJSON =
    Data.withObject
      "CustomPluginLocationDescription"
      ( \x ->
          CustomPluginLocationDescription'
            Prelude.<$> (x Data..:? "s3Location")
      )

instance
  Prelude.Hashable
    CustomPluginLocationDescription
  where
  hashWithSalt
    _salt
    CustomPluginLocationDescription' {..} =
      _salt `Prelude.hashWithSalt` s3Location

instance
  Prelude.NFData
    CustomPluginLocationDescription
  where
  rnf CustomPluginLocationDescription' {..} =
    Prelude.rnf s3Location
