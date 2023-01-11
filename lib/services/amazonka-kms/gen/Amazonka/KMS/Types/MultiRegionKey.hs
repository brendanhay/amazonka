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
-- Module      : Amazonka.KMS.Types.MultiRegionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.MultiRegionKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the primary or replica key in a multi-Region key.
--
-- /See:/ 'newMultiRegionKey' smart constructor.
data MultiRegionKey = MultiRegionKey'
  { -- | Displays the key ARN of a primary or replica key of a multi-Region key.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Displays the Amazon Web Services Region of a primary or replica key in a
    -- multi-Region key.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiRegionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'multiRegionKey_arn' - Displays the key ARN of a primary or replica key of a multi-Region key.
--
-- 'region', 'multiRegionKey_region' - Displays the Amazon Web Services Region of a primary or replica key in a
-- multi-Region key.
newMultiRegionKey ::
  MultiRegionKey
newMultiRegionKey =
  MultiRegionKey'
    { arn = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | Displays the key ARN of a primary or replica key of a multi-Region key.
multiRegionKey_arn :: Lens.Lens' MultiRegionKey (Prelude.Maybe Prelude.Text)
multiRegionKey_arn = Lens.lens (\MultiRegionKey' {arn} -> arn) (\s@MultiRegionKey' {} a -> s {arn = a} :: MultiRegionKey)

-- | Displays the Amazon Web Services Region of a primary or replica key in a
-- multi-Region key.
multiRegionKey_region :: Lens.Lens' MultiRegionKey (Prelude.Maybe Prelude.Text)
multiRegionKey_region = Lens.lens (\MultiRegionKey' {region} -> region) (\s@MultiRegionKey' {} a -> s {region = a} :: MultiRegionKey)

instance Data.FromJSON MultiRegionKey where
  parseJSON =
    Data.withObject
      "MultiRegionKey"
      ( \x ->
          MultiRegionKey'
            Prelude.<$> (x Data..:? "Arn") Prelude.<*> (x Data..:? "Region")
      )

instance Prelude.Hashable MultiRegionKey where
  hashWithSalt _salt MultiRegionKey' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` region

instance Prelude.NFData MultiRegionKey where
  rnf MultiRegionKey' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf region
