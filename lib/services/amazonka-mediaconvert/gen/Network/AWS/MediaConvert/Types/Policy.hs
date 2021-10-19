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
-- Module      : Network.AWS.MediaConvert.Types.Policy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Policy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.InputPolicy
import qualified Network.AWS.Prelude as Prelude

-- | A policy configures behavior that you allow or disallow for your
-- account. For information about MediaConvert policies, see the user guide
-- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | Allow or disallow jobs that specify Amazon S3 inputs.
    s3Inputs :: Prelude.Maybe InputPolicy,
    -- | Allow or disallow jobs that specify HTTP inputs.
    httpInputs :: Prelude.Maybe InputPolicy,
    -- | Allow or disallow jobs that specify HTTPS inputs.
    httpsInputs :: Prelude.Maybe InputPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Policy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Inputs', 'policy_s3Inputs' - Allow or disallow jobs that specify Amazon S3 inputs.
--
-- 'httpInputs', 'policy_httpInputs' - Allow or disallow jobs that specify HTTP inputs.
--
-- 'httpsInputs', 'policy_httpsInputs' - Allow or disallow jobs that specify HTTPS inputs.
newPolicy ::
  Policy
newPolicy =
  Policy'
    { s3Inputs = Prelude.Nothing,
      httpInputs = Prelude.Nothing,
      httpsInputs = Prelude.Nothing
    }

-- | Allow or disallow jobs that specify Amazon S3 inputs.
policy_s3Inputs :: Lens.Lens' Policy (Prelude.Maybe InputPolicy)
policy_s3Inputs = Lens.lens (\Policy' {s3Inputs} -> s3Inputs) (\s@Policy' {} a -> s {s3Inputs = a} :: Policy)

-- | Allow or disallow jobs that specify HTTP inputs.
policy_httpInputs :: Lens.Lens' Policy (Prelude.Maybe InputPolicy)
policy_httpInputs = Lens.lens (\Policy' {httpInputs} -> httpInputs) (\s@Policy' {} a -> s {httpInputs = a} :: Policy)

-- | Allow or disallow jobs that specify HTTPS inputs.
policy_httpsInputs :: Lens.Lens' Policy (Prelude.Maybe InputPolicy)
policy_httpsInputs = Lens.lens (\Policy' {httpsInputs} -> httpsInputs) (\s@Policy' {} a -> s {httpsInputs = a} :: Policy)

instance Core.FromJSON Policy where
  parseJSON =
    Core.withObject
      "Policy"
      ( \x ->
          Policy'
            Prelude.<$> (x Core..:? "s3Inputs")
            Prelude.<*> (x Core..:? "httpInputs")
            Prelude.<*> (x Core..:? "httpsInputs")
      )

instance Prelude.Hashable Policy

instance Prelude.NFData Policy

instance Core.ToJSON Policy where
  toJSON Policy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3Inputs" Core..=) Prelude.<$> s3Inputs,
            ("httpInputs" Core..=) Prelude.<$> httpInputs,
            ("httpsInputs" Core..=) Prelude.<$> httpsInputs
          ]
      )
