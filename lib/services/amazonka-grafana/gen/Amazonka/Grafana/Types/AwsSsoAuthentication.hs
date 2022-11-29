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
-- Module      : Amazonka.Grafana.Types.AwsSsoAuthentication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.AwsSsoAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about how this workspace works with
-- IAM Identity Center.
--
-- /See:/ 'newAwsSsoAuthentication' smart constructor.
data AwsSsoAuthentication = AwsSsoAuthentication'
  { -- | The ID of the IAM Identity Center-managed application that is created by
    -- Amazon Managed Grafana.
    ssoClientId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSsoAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssoClientId', 'awsSsoAuthentication_ssoClientId' - The ID of the IAM Identity Center-managed application that is created by
-- Amazon Managed Grafana.
newAwsSsoAuthentication ::
  AwsSsoAuthentication
newAwsSsoAuthentication =
  AwsSsoAuthentication'
    { ssoClientId =
        Prelude.Nothing
    }

-- | The ID of the IAM Identity Center-managed application that is created by
-- Amazon Managed Grafana.
awsSsoAuthentication_ssoClientId :: Lens.Lens' AwsSsoAuthentication (Prelude.Maybe Prelude.Text)
awsSsoAuthentication_ssoClientId = Lens.lens (\AwsSsoAuthentication' {ssoClientId} -> ssoClientId) (\s@AwsSsoAuthentication' {} a -> s {ssoClientId = a} :: AwsSsoAuthentication)

instance Core.FromJSON AwsSsoAuthentication where
  parseJSON =
    Core.withObject
      "AwsSsoAuthentication"
      ( \x ->
          AwsSsoAuthentication'
            Prelude.<$> (x Core..:? "ssoClientId")
      )

instance Prelude.Hashable AwsSsoAuthentication where
  hashWithSalt _salt AwsSsoAuthentication' {..} =
    _salt `Prelude.hashWithSalt` ssoClientId

instance Prelude.NFData AwsSsoAuthentication where
  rnf AwsSsoAuthentication' {..} =
    Prelude.rnf ssoClientId
