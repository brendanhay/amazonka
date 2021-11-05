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
-- Module      : Network.AWS.QuickSight.Types.AmazonOpenSearchParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.AmazonOpenSearchParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newAmazonOpenSearchParameters' smart constructor.
data AmazonOpenSearchParameters = AmazonOpenSearchParameters'
  { domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonOpenSearchParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'amazonOpenSearchParameters_domain' - Undocumented member.
newAmazonOpenSearchParameters ::
  -- | 'domain'
  Prelude.Text ->
  AmazonOpenSearchParameters
newAmazonOpenSearchParameters pDomain_ =
  AmazonOpenSearchParameters' {domain = pDomain_}

-- | Undocumented member.
amazonOpenSearchParameters_domain :: Lens.Lens' AmazonOpenSearchParameters Prelude.Text
amazonOpenSearchParameters_domain = Lens.lens (\AmazonOpenSearchParameters' {domain} -> domain) (\s@AmazonOpenSearchParameters' {} a -> s {domain = a} :: AmazonOpenSearchParameters)

instance Core.FromJSON AmazonOpenSearchParameters where
  parseJSON =
    Core.withObject
      "AmazonOpenSearchParameters"
      ( \x ->
          AmazonOpenSearchParameters'
            Prelude.<$> (x Core..: "Domain")
      )

instance Prelude.Hashable AmazonOpenSearchParameters

instance Prelude.NFData AmazonOpenSearchParameters

instance Core.ToJSON AmazonOpenSearchParameters where
  toJSON AmazonOpenSearchParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Domain" Core..= domain)]
      )
