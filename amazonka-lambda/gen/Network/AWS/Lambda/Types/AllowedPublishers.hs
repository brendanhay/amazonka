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
-- Module      : Network.AWS.Lambda.Types.AllowedPublishers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AllowedPublishers where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
        Lens._Coerce Lens.# pSigningProfileVersionArns_
    }

-- | The Amazon Resource Name (ARN) for each of the signing profiles. A
-- signing profile defines a trusted user who can sign a code package.
allowedPublishers_signingProfileVersionArns :: Lens.Lens' AllowedPublishers (Prelude.NonEmpty Prelude.Text)
allowedPublishers_signingProfileVersionArns = Lens.lens (\AllowedPublishers' {signingProfileVersionArns} -> signingProfileVersionArns) (\s@AllowedPublishers' {} a -> s {signingProfileVersionArns = a} :: AllowedPublishers) Prelude.. Lens._Coerce

instance Core.FromJSON AllowedPublishers where
  parseJSON =
    Core.withObject
      "AllowedPublishers"
      ( \x ->
          AllowedPublishers'
            Prelude.<$> (x Core..: "SigningProfileVersionArns")
      )

instance Prelude.Hashable AllowedPublishers

instance Prelude.NFData AllowedPublishers

instance Core.ToJSON AllowedPublishers where
  toJSON AllowedPublishers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SigningProfileVersionArns"
                  Core..= signingProfileVersionArns
              )
          ]
      )
