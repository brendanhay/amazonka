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
-- Module      : Amazonka.QuickSight.Types.ServiceNowParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ServiceNowParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for ServiceNow.
--
-- /See:/ 'newServiceNowParameters' smart constructor.
data ServiceNowParameters = ServiceNowParameters'
  { -- | URL of the base site.
    siteBaseUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'siteBaseUrl', 'serviceNowParameters_siteBaseUrl' - URL of the base site.
newServiceNowParameters ::
  -- | 'siteBaseUrl'
  Prelude.Text ->
  ServiceNowParameters
newServiceNowParameters pSiteBaseUrl_ =
  ServiceNowParameters' {siteBaseUrl = pSiteBaseUrl_}

-- | URL of the base site.
serviceNowParameters_siteBaseUrl :: Lens.Lens' ServiceNowParameters Prelude.Text
serviceNowParameters_siteBaseUrl = Lens.lens (\ServiceNowParameters' {siteBaseUrl} -> siteBaseUrl) (\s@ServiceNowParameters' {} a -> s {siteBaseUrl = a} :: ServiceNowParameters)

instance Data.FromJSON ServiceNowParameters where
  parseJSON =
    Data.withObject
      "ServiceNowParameters"
      ( \x ->
          ServiceNowParameters'
            Prelude.<$> (x Data..: "SiteBaseUrl")
      )

instance Prelude.Hashable ServiceNowParameters where
  hashWithSalt _salt ServiceNowParameters' {..} =
    _salt `Prelude.hashWithSalt` siteBaseUrl

instance Prelude.NFData ServiceNowParameters where
  rnf ServiceNowParameters' {..} =
    Prelude.rnf siteBaseUrl

instance Data.ToJSON ServiceNowParameters where
  toJSON ServiceNowParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SiteBaseUrl" Data..= siteBaseUrl)]
      )
