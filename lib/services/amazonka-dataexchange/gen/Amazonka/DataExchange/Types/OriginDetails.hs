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
-- Module      : Amazonka.DataExchange.Types.OriginDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.OriginDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the origin of the data set.
--
-- /See:/ 'newOriginDetails' smart constructor.
data OriginDetails = OriginDetails'
  { -- | The product ID of the origin of the data set.
    productId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productId', 'originDetails_productId' - The product ID of the origin of the data set.
newOriginDetails ::
  -- | 'productId'
  Prelude.Text ->
  OriginDetails
newOriginDetails pProductId_ =
  OriginDetails' {productId = pProductId_}

-- | The product ID of the origin of the data set.
originDetails_productId :: Lens.Lens' OriginDetails Prelude.Text
originDetails_productId = Lens.lens (\OriginDetails' {productId} -> productId) (\s@OriginDetails' {} a -> s {productId = a} :: OriginDetails)

instance Core.FromJSON OriginDetails where
  parseJSON =
    Core.withObject
      "OriginDetails"
      ( \x ->
          OriginDetails' Prelude.<$> (x Core..: "ProductId")
      )

instance Prelude.Hashable OriginDetails where
  hashWithSalt _salt OriginDetails' {..} =
    _salt `Prelude.hashWithSalt` productId

instance Prelude.NFData OriginDetails where
  rnf OriginDetails' {..} = Prelude.rnf productId
