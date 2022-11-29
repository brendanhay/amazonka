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
-- Module      : Amazonka.SecurityHub.Types.AwsApiCallActionDomainDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiCallActionDomainDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provided if @CallerType@ is @domain@. It provides information about the
-- DNS domain that issued the API call.
--
-- /See:/ 'newAwsApiCallActionDomainDetails' smart constructor.
data AwsApiCallActionDomainDetails = AwsApiCallActionDomainDetails'
  { -- | The name of the DNS domain that issued the API call.
    domain :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiCallActionDomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'awsApiCallActionDomainDetails_domain' - The name of the DNS domain that issued the API call.
newAwsApiCallActionDomainDetails ::
  AwsApiCallActionDomainDetails
newAwsApiCallActionDomainDetails =
  AwsApiCallActionDomainDetails'
    { domain =
        Prelude.Nothing
    }

-- | The name of the DNS domain that issued the API call.
awsApiCallActionDomainDetails_domain :: Lens.Lens' AwsApiCallActionDomainDetails (Prelude.Maybe Prelude.Text)
awsApiCallActionDomainDetails_domain = Lens.lens (\AwsApiCallActionDomainDetails' {domain} -> domain) (\s@AwsApiCallActionDomainDetails' {} a -> s {domain = a} :: AwsApiCallActionDomainDetails)

instance Core.FromJSON AwsApiCallActionDomainDetails where
  parseJSON =
    Core.withObject
      "AwsApiCallActionDomainDetails"
      ( \x ->
          AwsApiCallActionDomainDetails'
            Prelude.<$> (x Core..:? "Domain")
      )

instance
  Prelude.Hashable
    AwsApiCallActionDomainDetails
  where
  hashWithSalt _salt AwsApiCallActionDomainDetails' {..} =
    _salt `Prelude.hashWithSalt` domain

instance Prelude.NFData AwsApiCallActionDomainDetails where
  rnf AwsApiCallActionDomainDetails' {..} =
    Prelude.rnf domain

instance Core.ToJSON AwsApiCallActionDomainDetails where
  toJSON AwsApiCallActionDomainDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Domain" Core..=) Prelude.<$> domain]
      )
