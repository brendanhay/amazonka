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
-- Module      : Amazonka.Lambda.Types.AllowedPublishers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.AllowedPublishers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of signing profiles that can sign a code package.
--
-- /See:/ 'newAllowedPublishers' smart constructor.
data AllowedPublishers = AllowedPublishers'
  { -- | The Amazon Resource Name (ARN) for each of the signing profiles. A
    -- signing profile defines a trusted user who can sign a code package.
    signingProfileVersionArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowedPublishers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingProfileVersionArns', 'allowedPublishers_signingProfileVersionArns' - The Amazon Resource Name (ARN) for each of the signing profiles. A
-- signing profile defines a trusted user who can sign a code package.
newAllowedPublishers ::
  -- | 'signingProfileVersionArns'
  Prelude.NonEmpty Prelude.Text ->
  AllowedPublishers
newAllowedPublishers pSigningProfileVersionArns_ =
  AllowedPublishers'
    { signingProfileVersionArns =
        Lens.coerced Lens.# pSigningProfileVersionArns_
    }

-- | The Amazon Resource Name (ARN) for each of the signing profiles. A
-- signing profile defines a trusted user who can sign a code package.
allowedPublishers_signingProfileVersionArns :: Lens.Lens' AllowedPublishers (Prelude.NonEmpty Prelude.Text)
allowedPublishers_signingProfileVersionArns = Lens.lens (\AllowedPublishers' {signingProfileVersionArns} -> signingProfileVersionArns) (\s@AllowedPublishers' {} a -> s {signingProfileVersionArns = a} :: AllowedPublishers) Prelude.. Lens.coerced

instance Data.FromJSON AllowedPublishers where
  parseJSON =
    Data.withObject
      "AllowedPublishers"
      ( \x ->
          AllowedPublishers'
            Prelude.<$> (x Data..: "SigningProfileVersionArns")
      )

instance Prelude.Hashable AllowedPublishers where
  hashWithSalt _salt AllowedPublishers' {..} =
    _salt
      `Prelude.hashWithSalt` signingProfileVersionArns

instance Prelude.NFData AllowedPublishers where
  rnf AllowedPublishers' {..} =
    Prelude.rnf signingProfileVersionArns

instance Data.ToJSON AllowedPublishers where
  toJSON AllowedPublishers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SigningProfileVersionArns"
                  Data..= signingProfileVersionArns
              )
          ]
      )
