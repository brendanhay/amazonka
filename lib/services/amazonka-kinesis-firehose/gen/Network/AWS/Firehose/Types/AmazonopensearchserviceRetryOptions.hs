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
-- Module      : Network.AWS.Firehose.Types.AmazonopensearchserviceRetryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.AmazonopensearchserviceRetryOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceRetryOptions' smart constructor.
data AmazonopensearchserviceRetryOptions = AmazonopensearchserviceRetryOptions'
  { durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'amazonopensearchserviceRetryOptions_durationInSeconds' - Undocumented member.
newAmazonopensearchserviceRetryOptions ::
  AmazonopensearchserviceRetryOptions
newAmazonopensearchserviceRetryOptions =
  AmazonopensearchserviceRetryOptions'
    { durationInSeconds =
        Prelude.Nothing
    }

-- | Undocumented member.
amazonopensearchserviceRetryOptions_durationInSeconds :: Lens.Lens' AmazonopensearchserviceRetryOptions (Prelude.Maybe Prelude.Natural)
amazonopensearchserviceRetryOptions_durationInSeconds = Lens.lens (\AmazonopensearchserviceRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@AmazonopensearchserviceRetryOptions' {} a -> s {durationInSeconds = a} :: AmazonopensearchserviceRetryOptions)

instance
  Core.FromJSON
    AmazonopensearchserviceRetryOptions
  where
  parseJSON =
    Core.withObject
      "AmazonopensearchserviceRetryOptions"
      ( \x ->
          AmazonopensearchserviceRetryOptions'
            Prelude.<$> (x Core..:? "DurationInSeconds")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceRetryOptions

instance
  Prelude.NFData
    AmazonopensearchserviceRetryOptions

instance
  Core.ToJSON
    AmazonopensearchserviceRetryOptions
  where
  toJSON AmazonopensearchserviceRetryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Core..=)
              Prelude.<$> durationInSeconds
          ]
      )
