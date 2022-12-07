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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAmazonopensearchserviceBufferingHints' smart constructor.
data AmazonopensearchserviceBufferingHints = AmazonopensearchserviceBufferingHints'
  { sizeInMBs :: Prelude.Maybe Prelude.Natural,
    intervalInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceBufferingHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMBs', 'amazonopensearchserviceBufferingHints_sizeInMBs' - Undocumented member.
--
-- 'intervalInSeconds', 'amazonopensearchserviceBufferingHints_intervalInSeconds' - Undocumented member.
newAmazonopensearchserviceBufferingHints ::
  AmazonopensearchserviceBufferingHints
newAmazonopensearchserviceBufferingHints =
  AmazonopensearchserviceBufferingHints'
    { sizeInMBs =
        Prelude.Nothing,
      intervalInSeconds = Prelude.Nothing
    }

-- | Undocumented member.
amazonopensearchserviceBufferingHints_sizeInMBs :: Lens.Lens' AmazonopensearchserviceBufferingHints (Prelude.Maybe Prelude.Natural)
amazonopensearchserviceBufferingHints_sizeInMBs = Lens.lens (\AmazonopensearchserviceBufferingHints' {sizeInMBs} -> sizeInMBs) (\s@AmazonopensearchserviceBufferingHints' {} a -> s {sizeInMBs = a} :: AmazonopensearchserviceBufferingHints)

-- | Undocumented member.
amazonopensearchserviceBufferingHints_intervalInSeconds :: Lens.Lens' AmazonopensearchserviceBufferingHints (Prelude.Maybe Prelude.Natural)
amazonopensearchserviceBufferingHints_intervalInSeconds = Lens.lens (\AmazonopensearchserviceBufferingHints' {intervalInSeconds} -> intervalInSeconds) (\s@AmazonopensearchserviceBufferingHints' {} a -> s {intervalInSeconds = a} :: AmazonopensearchserviceBufferingHints)

instance
  Data.FromJSON
    AmazonopensearchserviceBufferingHints
  where
  parseJSON =
    Data.withObject
      "AmazonopensearchserviceBufferingHints"
      ( \x ->
          AmazonopensearchserviceBufferingHints'
            Prelude.<$> (x Data..:? "SizeInMBs")
            Prelude.<*> (x Data..:? "IntervalInSeconds")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceBufferingHints
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceBufferingHints' {..} =
      _salt `Prelude.hashWithSalt` sizeInMBs
        `Prelude.hashWithSalt` intervalInSeconds

instance
  Prelude.NFData
    AmazonopensearchserviceBufferingHints
  where
  rnf AmazonopensearchserviceBufferingHints' {..} =
    Prelude.rnf sizeInMBs
      `Prelude.seq` Prelude.rnf intervalInSeconds

instance
  Data.ToJSON
    AmazonopensearchserviceBufferingHints
  where
  toJSON AmazonopensearchserviceBufferingHints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SizeInMBs" Data..=) Prelude.<$> sizeInMBs,
            ("IntervalInSeconds" Data..=)
              Prelude.<$> intervalInSeconds
          ]
      )
